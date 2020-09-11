use bit_vec::BitVec;
use bitflags::bitflags;
use crc32fast::Hasher;
use std::num::NonZeroU32;

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

struct GZExtraRecord {
  si1: u8,
  si2: u8,
  len: u16,
  data: [u8],
}

struct GZExtra {
  xlen: u16,
  data: [u8], // Unsafe to access directly--should use member functions
}

pub struct GzipData {
  id1: u8,
  id2: u8,
  cm: u8,
  flg: GZFlags,
  mtime: Option<NonZeroU32>,
  xfl: GZXFlags,
  os: OSType,
  fextra: Option<Box<GZExtra>>,
  fname: Option<String>,
  comment: Option<String>,
  crc16: Option<u16>,
  crc32: u32,
  isize: u32,
  data: BitVec,
}

pub fn calc_crc32(data: &BitVec) -> u32 {
  let byte_data = data.to_bytes();
  let mut hasher = Hasher::new();
  hasher.update(byte_data.as_slice());
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
      fextra: None,
      fname: None,
      comment: None,
      crc16: None,
      crc32: u32::MAX,
      isize: 0,
      data: BitVec::new(),
    }
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
    assert_eq!(invalid1.is_valid(), false);
    assert_eq!(invalid1.is_valid(), false);
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
