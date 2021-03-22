pub mod reader;
pub mod writer;

use bitflags::bitflags;
use bitstream_io::{ByteWrite, ByteWriter, LittleEndian};
use crc32fast::Hasher;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use std::io::Write as IOWrite;
use std::num::NonZeroU32;
use std::{
  convert::TryInto,
  fmt::{self, Write},
};

/* Structure from RFC 1952

Header
   +---+---+---+---+---+---+---+---+---+---+
   |ID1|ID2|CM |FLG|     MTIME     |XFL|OS | (more-->)
   +---+---+---+---+---+---+---+---+---+---+
(if FLG.FEXTRA set)

   +---+---+=================================+
   | XLEN  |...XLEN bytes of "extra field"...| (more-->)
   +---+---+=================================+

(if FLG.FNAME set)

   +=========================================+
   |...original file name, zero-terminated...| (more-->)
   +=========================================+

(if FLG.FCOMMENT set)

   +===================================+
   |...file comment, zero-terminated...| (more-->)
   +===================================+

(if FLG.FHCRC set)

   +---+---+
   | CRC16 |
   +---+---+

   +=======================+
   |...compressed blocks...| (more-->)
   +=======================+

     0   1   2   3   4   5   6   7
   +---+---+---+---+---+---+---+---+
   |     CRC32     |     ISIZE     |
   +---+---+---+---+---+---+---+---+
*/

pub const GZIP_ID1: u8 = 0x1f;
pub const GZIP_ID2: u8 = 0x8b;

bitflags! {
    /** The bitfields corresponding to FLG in RFC 1952 */
    #[derive(Default)]
    pub struct GZFlags: u8 {
        const FTEXT     = 0b1 << 0;
        const FHCRC     = 0b1 << 1;
        const FEXTRA    = 0b1 << 2;
        const FNAME     = 0b1 << 3;
        const FCOMMENT  = 0b1 << 4;
        const RESERVED1 = 0b1 << 5;
        const RESERVED2 = 0b1 << 6;
        const RESERVED3 = 0b1 << 7;
    }
}
bitflags! {
    #[derive(Default)]
    /** The bitfields corresponding to XFL in RGC 1952 */
    pub struct GZXFlags: u8 {
        const XFL_NO_FLAGS = 0;
        const XFL_MAX_COMPRESSION = 2;
        const XFL_MAX_SPEED = 4;
    }
}

#[derive(Debug, Eq, PartialEq, TryFromPrimitive, IntoPrimitive, Copy, Clone)]
#[repr(u8)]
pub enum OSType {
  FatFS = 0,
  Amiga = 1,
  VMS = 2,
  Unix = 3,
  VMorCMS = 4,
  AtariTOS = 5,
  HpfsFS = 6,
  Macintosh = 7,
  ZSystem = 8,
  CPM = 9,
  Tops20 = 10,
  NtfsFS = 11,
  Qds = 12,
  AcornRISC = 13,
  Unknown = 255,
}

impl GZFlags {
  pub fn is_valid(&self) -> bool {
    let all_invalid_bits = GZFlags::from_bits_truncate(0b11100000);
    !self.intersects(all_invalid_bits)
  }
}

#[derive(Debug)]
struct GZExtraRecord {
  si1: u8,
  si2: u8,
  len: u16,
  data: [u8],
}

#[derive(Debug)]
struct GZExtra {
  xlen: u16,
  data: [u8], // Unsafe to access directly--should use member functions
}

#[derive(Debug, Clone)]
pub struct GzipData {
  id1: u8,
  id2: u8,
  cm: u8,
  flg: GZFlags,
  mtime: Option<NonZeroU32>,
  xfl: GZXFlags,
  os: OSType,
  fextra: (),
  fname: Option<String>,
  comment: Option<String>,
  crc16: Option<u16>,
  crc32: u32,
  isz: u32,
  data: Vec<u8>,
}

pub fn calc_crc32(data: &[u8]) -> u32 {
  let mut hasher = Hasher::new();
  hasher.update(data);
  hasher.finalize()
}

impl GzipData {
  /// Create a new GZipData which represents an empty file
  pub fn new() -> GzipData {
    Self {
      id1: GZIP_ID1,
      id2: GZIP_ID2,
      cm: 8,
      flg: GZFlags::empty(),
      mtime: None,
      xfl: GZXFlags::empty(),
      os: OSType::Unknown,
      fextra: (),
      fname: None,
      comment: None,
      crc16: None,
      crc32: u32::MAX,
      isz: 0,
      data: Vec::new(),
    }
  }

  pub fn write_to<W: IOWrite>(&self, out: W) -> std::io::Result<W> {
    let mut bytesout: ByteWriter<W, LittleEndian> = ByteWriter::new(out);
    bytesout.write(self.id1)?;
    bytesout.write(self.id2)?;
    bytesout.write(self.cm)?;
    bytesout.write(0u8)?; // Since we can't convert flg into a u8 (thanks!)
                          // just don't set any of the extra flags.
    bytesout.write(self.mtime.map(u32::from).unwrap_or(0u32))?;
    bytesout.write(0u8)?; // Same for XFL
    bytesout.write(u8::from(self.os))?;
    bytesout.write_bytes(&self.data[..])?;
    bytesout.write(self.crc32)?;
    bytesout.write(self.isz)?;
    Ok(bytesout.into_writer())
  }

  pub fn get_checksums(&self) -> (u32, u32) {
    (self.crc32, self.isz)
  }

  /** Take ownership of a BitVector of compressed data, updating the associated
  metadata fields appropriately */
  pub fn set_uncompressed_data(&mut self, data: Vec<u8>) {
    let isz = data.len().try_into().unwrap();
    let crc32 = calc_crc32(&data[..]);
    self.data = data;
    self.isz = isz;
    self.crc32 = crc32;
  }

  pub fn set_compressed_data(&mut self, data: Vec<u8>) {
    self.data = data;
  }

  pub fn set_checksums(&mut self, crc32: u32, isz: u32) {
    self.crc32 = crc32;
    self.isz = isz;
  }

  pub fn set_flags(&mut self, flg: GZFlags) {
    self.flg = flg;
  }

  pub fn set_xflags(&mut self, xflg: GZXFlags) {
    self.xfl = xflg;
  }

  pub fn set_ostype(&mut self, os: OSType) {
    self.os = os;
  }

  pub fn set_mtime(&mut self, mtime: Option<NonZeroU32>) {
    self.mtime = mtime;
  }

  pub fn set_name(&mut self, name: Option<String>) {
    self.fname = name;
  }

  pub fn set_comment(&mut self, comment: Option<String>) {
    self.comment = comment;
  }

  pub fn get_data_copy(&self) -> Vec<u8> {
    self.data.clone()
  }
  pub fn into_data(self) -> Vec<u8> {
    self.data
  }

  // Intentionally ignore EXTRA for the moment.

  pub fn fmt_header<W: Write>(&self, f: &mut W) -> fmt::Result {
    write!(
      f,
      r#"GZip Data:
   +---+---+---+---+---+---+---+---+---+---+
   |ID1|ID2|CM |FLG|     MTIME     |XFL|OS | (more-->)
   +---+---+---+---+---+---+---+---+---+---
   {:x}   {:x}  {}   {:02x}  {:x?}   {:x}  {}
"#,
      self.id1, self.id2, self.cm, self.flg, self.mtime, self.xfl, self.os as u8
    )?;
    if self.flg.contains(GZFlags::FEXTRA) {
      writeln!(f, "Had an EXTRA field")?;
    }

    if self.flg.contains(GZFlags::FNAME) {
      writeln!(f, "Filename: {}", self.fname.as_ref().unwrap())?;
    }

    if self.flg.contains(GZFlags::FCOMMENT) {
      writeln!(f, "Comment: {}", self.comment.as_ref().unwrap())?;
    }

    if self.flg.contains(GZFlags::FHCRC) {
      writeln!(f, "CRC16: {:x}", self.crc16.unwrap())?;
    }

    writeln!(f, "CRC32: {:x?}", self.crc32)?;
    writeln!(f, "Num Bytes: {}", self.isz)
  }

  pub fn fmt_data<W: Write>(&self, f: &mut W) -> fmt::Result {
    let mut i = 0usize;
    write!(f, "Data: [")?;
    'dataprint: loop {
      for _ in 0..20 {
        if i >= self.data.len() {
          break 'dataprint;
        }
        write!(f, "{:02x?} ", self.data[i])?;
        i += 1;
      }
      writeln!(f)?;
    }
    writeln!(f, "];")
  }
}

impl fmt::Display for GzipData {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.fmt_header(f)?;
    self.fmt_data(f)
  }
}

impl Default for GzipData {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
  use super::GZFlags;

  #[test]
  fn test_invalid_gzflags() {
    let invalid1 = GZFlags::RESERVED1;
    let invalid2 = GZFlags::RESERVED2;
    let invalid3 = GZFlags::RESERVED3;
    assert_eq!(invalid1.is_valid(), false);
    assert_eq!(invalid2.is_valid(), false);
    assert_eq!(invalid3.is_valid(), false);
  }

  #[test]
  fn test_valid_gzflags() {
    let v1 = GZFlags::FTEXT;
    let v2 = GZFlags::FHCRC;
    let v3 = GZFlags::FEXTRA;
    let v4 = GZFlags::FNAME;
    let v5 = GZFlags::FCOMMENT;
    assert_eq!(v1.is_valid(), true);
    assert_eq!(v2.is_valid(), true);
    assert_eq!(v3.is_valid(), true);
    assert_eq!(v4.is_valid(), true);
    assert_eq!(v5.is_valid(), true);
  }
}
