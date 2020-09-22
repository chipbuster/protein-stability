pub use super::{calc_crc32, GZFlags, GZXFlags, GzipData, OSType, GZIP_ID1, GZIP_ID2};
use crate::deflate::decoder::DeflateReadError;
use crate::deflate::DeflateStream;
use std::convert::{TryFrom, TryInto};
use std::io;
use std::io::{BufRead, BufReader, ErrorKind, Read};
use std::num::NonZeroU32;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum GzipReadError {
  #[error("gzip magic bytes invalid. Expected 1F 8B, got {:x} {:x}", .0, .1)]
  InvalidMagicBytes(u8, u8),
  #[error("gzip compression byte invalid. Expected 8, got {0}")]
  InvalidCMByte(u8),
  #[error("Reserved bits on GZFlags were set: {:b}", .0)]
  InvalidFLGs(GZFlags),
  #[error("CRC32 mismatch: record says {:x}, but calculated {:x}", .0, .1)]
  CRC32Mismatch(u32, u32),
  #[error("Data size mismatch: record says {} bytes, but data has {} bytes", .0, .1)]
  IsizeMismatch(u32, u32),
  #[error("File Read Error: {:?}", .0)]
  FileReadError(#[from] io::Error),
}

impl GzipData {
  /// Create a new GZipData from a BitStream which specifies an existing file
  pub fn new_from_gzip_data<R: Read>(data: R) -> Result<Self, GzipReadError> {
    let mut buf_rdr = BufReader::new(data);

    let mut reqd_header = [0u8; 10];
    buf_rdr.read_exact(&mut reqd_header)?;

    let id1 = reqd_header[0];
    let id2 = reqd_header[1];
    let cm = reqd_header[2];
    let flags = GZFlags::from_bits(reqd_header[3]).unwrap();
    let mtime_raw = u32::from_le_bytes(reqd_header[4..8].try_into().unwrap());
    let mtime = NonZeroU32::new(mtime_raw);
    let xflags = GZXFlags::from_bits_truncate(reqd_header[8]);
    let os = OSType::try_from(reqd_header[9]).unwrap_or(OSType::Unknown);

    // Check to make sure header is valid
    if id1 != 0x1f || id2 != 0x8b {
      return Err(GzipReadError::InvalidMagicBytes(id1, id2));
    }
    if cm != 8 {
      return Err(GzipReadError::InvalidCMByte(cm));
    }
    if !flags.is_valid() {
      return Err(GzipReadError::InvalidFLGs(flags));
    }

    /* Ignore FTEXT: we don't care if it's set or not since it's heuristic.
    Remaining flags control processing and need to be checked. */
    let has_crc16 = flags.contains(GZFlags::FHCRC);
    let has_extra = flags.contains(GZFlags::FEXTRA);
    let has_name = flags.contains(GZFlags::FNAME);
    let has_comment = flags.contains(GZFlags::FCOMMENT);

    // Process the extra data for FTEXT, discarding it in the process.
    if has_extra {
      let mut xlen_bytes = [0u8; 2];
      buf_rdr.read_exact(&mut xlen_bytes)?;
      let xlen = u16::from_le_bytes(xlen_bytes);
      let mut extra = vec![0u8; xlen as usize];
      buf_rdr.read_exact(&mut extra)?;
      // Discard the extra data.
    }

    // Process name and comment, if needed
    let mut name = Vec::new();
    if has_name {
      buf_rdr.read_until(0u8, &mut name)?;
    }
    let name = std::str::from_utf8(&name)
      .expect("Name is not valid UTF-8")
      .to_owned();
    let mut comment = Vec::new();
    if has_comment {
      buf_rdr.read_until(0u8, &mut comment)?;
    }
    let comment = std::str::from_utf8(&comment)
      .expect("Comment is not valid UTF-8")
      .to_owned();

    // CRC16 checking is used on the header, not the contents. Ignore for now.
    let _crc16 = if has_crc16 {
      let mut crc_bytes = [0u8; 2];
      buf_rdr.read_exact(&mut crc_bytes)?;
      Some(u16::from_le_bytes(crc_bytes))
    } else {
      None
    };

    // At this point, we have everything but the data, crc32, and isize. Since
    // the latter two are in a trailer, we just read in everything left and
    // pick out the relevant fields on the end.
    let mut remainder = Vec::new();
    buf_rdr.read_to_end(&mut remainder)?;

    let num_bytes = remainder.len();
    if num_bytes < 8 {
      return Err(GzipReadError::FileReadError(io::Error::new(
        ErrorKind::UnexpectedEof,
        "Not enough data for CRC32/ISIZE trailer",
      )));
    }
    let isize_bytes = remainder.split_off(remainder.len() - 4);
    println!("{:?}", isize_bytes);
    let crc32_bytes = remainder.split_off(remainder.len() - 4);
    let isz = u32::from_le_bytes(isize_bytes.as_slice().try_into().unwrap());
    let crc32 = u32::from_le_bytes(crc32_bytes.as_slice().try_into().unwrap());

    let deflate_data = remainder;

    // Check if CRC matches
    let _my_crc32 = calc_crc32(&deflate_data);
    let _my_isize = deflate_data.len() as u32;

    /*
    size and CRC checks are for the uncompressed data, so these checks
    cannot be implemented until DEFLATE decompression is implemented.

    CRC16 checking could also be implemented in this block, but crc16 checks the
    header bytes, not the data bytes.

    if my_isize != isz {
      return Err(GzipReadError::IsizeMismatch(isz, my_isize));
    }
    if my_crc32 != crc32 {
      return Err(GzipReadError::CRC32Mismatch(crc32, my_crc32));
    }*/

    let mut output = GzipData::new();
    output.set_data(deflate_data);
    output.set_checksums(crc32, isz);
    output.set_flags(flags);
    output.set_xflags(xflags);
    output.set_mtime(mtime);
    output.set_ostype(os);
    if has_name {
      output.set_name(Some(name));
    }
    if has_comment {
      output.set_comment(Some(comment));
    }

    // Do not set name, comment, or extra

    Ok(output)
  }

  // Consume this reader to create a
  pub fn into_decoded(self) -> Result<Vec<u8>, DeflateReadError> {
    let stream = DeflateStream::new_from_source(self.data.as_slice())?;
    let crc32 = self.crc32;
    let isz = self.isz;
    stream.into_byte_stream_checked(crc32, isz)
  }
}

mod tests {
  #[allow(unused_imports)]
  use super::*;

  #[test]
  fn decode_compressed_hello() {
    // A gzip file with filename "simplehello.txt" containing the text "Hello!\n"
    let coded = [
      0x1f, 0x8b, 0x08, 0x08, 0xcf, 0x01, 0x69, 0x5f, 0x02, 0x03, 0x73, 0x69, 0x6d, 0x70, 0x6c,
      0x65, 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x2e, 0x74, 0x78, 0x74, 0x00, 0xf3, 0x48, 0xcd, 0xc9,
      0xc9, 0x57, 0xe4, 0x02, 0x00, 0x9e, 0xd8, 0x42, 0xb0, 0x07, 0x00, 0x00, 0x00u8,
    ];

    let gzip = GzipData::new_from_gzip_data(&coded[..]).unwrap();

    assert_eq!(gzip.fname, Some("simplehello.txt\u{0}".to_owned()));

    let output = gzip.into_decoded().unwrap();
    let outstr = std::str::from_utf8(&output).unwrap();
    assert_eq!(outstr, "Hello!\n");
  }
}
