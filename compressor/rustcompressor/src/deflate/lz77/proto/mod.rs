use crate::deflate::LZSym;
use std::convert::{From, TryInto};

pub mod proto {
  pub type TEST = i32;

  include!(concat!(env!("OUT_DIR"), "/lz77.rs"));
}

fn deflatesym_to_underlying(s: &LZSym<u64>) -> proto::deflate_sym::Sym {
  use proto::deflate_sym::Sym;
  use proto::{Backref, Literal};
  match s {
    LZSym::EndOfBlock => Sym::Lit(Literal { value: 255 }),
    LZSym::Literal(v) => Sym::Lit(Literal {
      value: u64::from(*v),
    }),
    LZSym::Backreference(length, dist) => Sym::Backref(Backref {
      length: *length,
      distance: *dist,
    }),
  }
}

pub fn deflatesym_to_proto<T: TryInto<u64> + Copy>(s: &LZSym<T>) -> Option<proto::DeflateSym> {
  let converted = match s {
    LZSym::Backreference(l, d) => {
      let ln = TryInto::<u64>::try_into(*l).ok()?;
      let dn = TryInto::<u64>::try_into(*d).ok()?;
      LZSym::Backreference(ln, dn)
    }
    // Have to write these explicitly to perform type conversions
    LZSym::EndOfBlock => LZSym::EndOfBlock,
    LZSym::Literal(x) => LZSym::Literal(*x),
  };

  Some(proto::DeflateSym {
    sym: Some(deflatesym_to_underlying(&converted)),
  })
}
