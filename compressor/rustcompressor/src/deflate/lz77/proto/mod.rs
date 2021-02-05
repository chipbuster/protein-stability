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
      value: u32::from(*v),
    }),
    DeflateSym::Backreference(length, dist) => Sym::Backref(Backref {
      length: u32::from(*length),
      distance: u32::from(*dist),
    }),
    DeflateSym::OffsetBackref(off, length, dist) => Sym::Offset(OffsetBackref {
      offset: u32::from(*off),
      length: u32::from(*length),
      distance: u32::from(*dist),
    }),
  }
}

pub fn deflatesym_to_proto(s: &DeflateSym) -> proto::DeflateSym {
  proto::DeflateSym {
    sym: Some(deflatesym_to_underlying(s)),
  }
}
