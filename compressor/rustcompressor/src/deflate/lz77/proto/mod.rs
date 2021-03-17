use crate::deflate::DeflateSym;
use std::convert::From;

pub mod proto {
  pub type TEST = i32;

  include!(concat!(env!("OUT_DIR"), "/lz77.rs"));
}

fn deflatesym_to_underlying(s: &DeflateSym) -> proto::deflate_sym::Sym {
  use proto::deflate_sym::Sym;
  use proto::{Backref, Literal, OffsetBackref};
  match s {
    DeflateSym::EndOfBlock => Sym::Lit(Literal { value: 255 }),
    DeflateSym::Literal(v) => Sym::Lit(Literal {
      value: u64::from(*v),
    }),
    DeflateSym::Backreference(length, dist) => Sym::Backref(Backref {
      length: u64::from(*length),
      distance: u64::from(*dist),
    }),
    DeflateSym::OffsetBackref(off, length, dist) => Sym::Offset(OffsetBackref {
      offset: u64::from(*off),
      length: u64::from(*length),
      distance: u64::from(*dist),
    }),
  }
}

pub fn deflatesym_to_proto(s: &DeflateSym) -> proto::DeflateSym {
  proto::DeflateSym {
    sym: Some(deflatesym_to_underlying(s)),
  }
}
