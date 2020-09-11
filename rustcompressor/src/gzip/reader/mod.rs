use super::common::{calc_crc32, GZFlags, GZXFlags, GzipData, OSType, GZIP_ID1, GZIP_ID2};
use bit_vec::BitVec;
use std::convert::TryFrom;
use std::io;
use std::io::Read;
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
}

impl GzipData {
  /// Create a new GZipData from a BitStream which specifies an existing file
  pub fn new_from_gzip_data<R: Read>(mut data: R) -> Result<Self, GzipReadError> {
    let mut reqd_header = [0u8; 4];
    data
      .read_exact(&mut reqd_header)
      .expect("Could not read header bytes");

    Ok(Self::new())
  }
}
