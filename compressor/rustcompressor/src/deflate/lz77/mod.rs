pub mod decoder;
pub mod encoder;
pub mod proto;

use super::DeflateSym;

pub fn encode_to_deflatesym(data: &[u8], use_offset: bool) -> Vec<DeflateSym> {
  encoder::do_lz77(data, use_offset)
}

/// Encode a sequence of bytes into a proto::Compressed message
pub fn encode_to_cmsg(data: &[u8], use_offset: bool) -> proto::proto::Compressed {
  let syms = encode_to_deflatesym(data, use_offset)
    .iter()
    .map(proto::deflatesym_to_proto)
    .collect();
  let data = data.to_vec();
  proto::proto::Compressed {
    rawbytes: data,
    syms,
  }
}
