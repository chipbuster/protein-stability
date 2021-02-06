pub mod decoder;
pub mod encoder;
pub mod proto;

use proto::deflatesym_to_proto;

use super::{DeflateStream, DeflateSym};

pub fn encode_to_deflatesym(data: &[u8], use_offset: bool) -> Vec<DeflateSym> {
  encoder::do_lz77(data, use_offset)
}

/// Encode a sequence of bytes into a proto::Compressed message
pub fn encode_deflate_to_cmsg(data: &DeflateStream) -> proto::proto::Compressed {
  // Construct encoded proto syms
  let mut symstream = Vec::new();
  for b in data.blocks.iter() {
    match &b.data {
      super::BlockData::Raw(u) => {
        let literal = u
          .data
          .iter()
          .cloned()
          .map(DeflateSym::Literal)
          .collect::<Vec<_>>();
        symstream.extend_from_slice(&literal[..]);
      }
      super::BlockData::Fix(c) | super::BlockData::Dyn(c) => {
        symstream.extend_from_slice(&c.data[..]);
      }
    }
  }
  let syms = symstream.iter().map(deflatesym_to_proto).collect();

  // Construct raw data
  let mut decoded = Vec::new();
  decoder::decode_lz77(&symstream[..], &mut decoded).unwrap();

  proto::proto::Compressed {
    rawbytes: decoded,
    syms,
  }
}
