use super::super::{BlockData, DeflateStream, DeflateSym, LZ77SymStream, LZSym};
use super::encoder::{do_lz77, LZRules};
use super::proto::deflatesym_to_proto;
use num::Bounded;
use std::convert::{TryFrom, TryInto};

impl LZ77SymStream {
  pub fn from_deflatestream(stream: &DeflateStream) -> Self {
    let mut symstream = Vec::new();
    for b in stream.blocks.iter() {
      match &b.data {
        BlockData::Raw(u) => {
          let literal = u
            .data
            .iter()
            .cloned()
            .map(LZSym::Literal::<u64>)
            .collect::<Vec<_>>();
          symstream.extend_from_slice(&literal[..]);
        }
        BlockData::Fix(c) | BlockData::Dyn(c) => {
          symstream.extend(c.data.iter().cloned().map(deflatesym_to_u64sym));
        }
      }
    }
    Self { symbols: symstream }
  }
  /// Generate a symbol stream by compressing raw bytes using the lz77 parameters
  pub fn from_uncompressed_bytes(data: &[u8], lzrules: &LZRules) -> Self {
    Self {
      symbols: do_lz77(data, lzrules),
    }
  }

  /// Encode a sequence of bytes into a proto::Compressed message
  pub fn to_cmsg(&self) -> super::proto::proto::Compressed {
    // Construct encoded proto syms
    let symstream = self
      .symbols
      .iter()
      .map(deflatesym_to_proto)
      .map(Option::unwrap)
      .collect::<Vec<_>>();

    // Construct raw data
    let nbytes_decoded = self
      .symbols
      .iter()
      .map(|x| {
        // How many bytes (symbols) result when this stream is decoded?
        match x {
          LZSym::Literal(_) => 1u64,
          LZSym::Backreference(l, _) => (*l).try_into().ok().unwrap(),
          LZSym::OffsetBackref(_, l, _) => (*l).try_into().ok().unwrap(),
          LZSym::EndOfBlock => 0,
        }
      })
      .sum();

    super::proto::proto::Compressed {
      nbytes_decoded,
      syms: symstream,
    }
  }
}

fn deflatesym_to_u64sym(x: DeflateSym) -> LZSym<u64> {
  match x {
    LZSym::EndOfBlock => LZSym::EndOfBlock,
    LZSym::Literal(x) => LZSym::Literal(x),
    LZSym::Backreference(l, d) => LZSym::Backreference(u64::from(l), u64::from(d)),
    LZSym::OffsetBackref(o, l, d) => LZSym::OffsetBackref(o, u64::from(l), u64::from(d)),
  }
}
