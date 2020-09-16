pub use super::common::{calc_crc32, GZFlags, GZXFlags, GzipData, OSType, GZIP_ID1, GZIP_ID2};

impl GzipData {
  /// Create a new GZipData from a BitVec which specifies the deflate-compressed data
  pub fn new_from_deflate_data(data: Vec<u8>) -> GzipData {
    let mut gzd = GzipData::new();
    gzd.set_data(data);
    gzd
  }
}
