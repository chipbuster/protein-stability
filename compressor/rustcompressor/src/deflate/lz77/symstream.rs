use super::proto::deflatesym_to_proto;

use super::encoder::{do_lz77, MaxMatchParameters};

use super::super::{BlockData, DeflateStream, DeflateSym, LZ77SymStream};

impl LZ77SymStream {
  /// Generate a symbol stream by compressing raw bytes using the lz77 parameters
  pub fn from_uncompressed_bytes(
    data: &[u8],
    maxmatch: &MaxMatchParameters,
    use_offset: bool,
  ) -> Self {
    Self {
      symbols: do_lz77(data, maxmatch, use_offset),
    }
  }

  /// Generate an LZ77Stream from DeflateStream, allowing
  pub fn from_deflatestream(stream: &DeflateStream) -> Self {
    let mut symstream = Vec::new();
    for b in stream.blocks.iter() {
      match &b.data {
        BlockData::Raw(u) => {
          let literal = u
            .data
            .iter()
            .cloned()
            .map(DeflateSym::Literal)
            .collect::<Vec<_>>();
          symstream.extend_from_slice(&literal[..]);
        }
        BlockData::Fix(c) | BlockData::Dyn(c) => {
          symstream.extend_from_slice(&c.data[..]);
        }
      }
    }
    Self { symbols: symstream }
  }

  /// Encode a sequence of bytes into a proto::Compressed message
  pub fn to_cmsg(&self) -> super::proto::proto::Compressed {
    // Construct encoded proto syms
    let symstream = self
      .symbols
      .iter()
      .map(deflatesym_to_proto)
      .collect::<Vec<_>>();

    // Construct raw data
    let nbytes_decoded = self
      .symbols
      .iter()
      .map(|x| {
        // How many bytes (symbols) result when this stream is decoded?
        match x {
          DeflateSym::Literal(_) => 1u64,
          DeflateSym::Backreference(l, _) => u64::from(*l),
          DeflateSym::OffsetBackref(_, l, _) => u64::from(*l),
          DeflateSym::EndOfBlock => 0,
        }
      })
      .sum();

    super::proto::proto::Compressed {
      nbytes_decoded,
      syms: symstream,
    }
  }
}
