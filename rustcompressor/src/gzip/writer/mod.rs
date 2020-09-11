use super::common::{calc_crc32, GZFlags, GZXFlags, GzipData, OSType, GZIP_ID1, GZIP_ID2};
use bit_vec::BitVec;
use std::convert::TryFrom;

impl GzipData {
  /// Create a new GZipData from a BitVec which specifies the deflate-compressed data
  pub fn new_from_deflate_data(data: BitVec) -> GzipData {
    let dsize = u32::try_from(data.to_bytes().len()).expect("Data too large");
    GzipData {
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
      crc32: calc_crc32(&data),
      isize: dsize,
      data: data,
    }
  }
}
