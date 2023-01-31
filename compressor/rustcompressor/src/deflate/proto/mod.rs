use crate::deflate::codepoints::{MAX_DIST_CODE, MIN_DIST_CODE, OFFSET_SIGIL};
use crate::deflate::*;
use std::{convert::TryFrom, unimplemented};

pub mod proto {
  pub type TEST = i32;

  include!(concat!(env!("OUT_DIR"), "/deflate_proto.rs"));
}

impl DeflateSym {
  fn underlying(self) -> proto::deflate_sym::Sym {
    use proto::deflate_sym::Sym;
    use proto::{Backref, Literal, OffsetBackref};
    match self {
      Self::EndOfBlock => Sym::Lit(Literal { value: 255 }),
      Self::Literal(v) => Sym::Lit(Literal { value: v.into() }),
      Self::Backreference(length, dist) => Sym::Backref(Backref {
        length: length.into(),
        distance: dist.into(),
      }),
      Self::OffsetBackref(off, length, dist) => Sym::Offset(OffsetBackref {
        offset: off.into(),
        length: length.into(),
        distance: dist.into(),
      }),
    }
  }

  pub fn into_proto(self) -> proto::DeflateSym {
    proto::DeflateSym {
      sym: Some(self.underlying()),
    }
  }
}

impl UncompressedBlock {
  pub fn into_proto(self) -> proto::UncompressedBlock {
    proto::UncompressedBlock { data: self.data }
  }
}

impl CompressedBlock {
  fn dict_to_bitsused(d: CodeDict<u16>) -> std::collections::HashMap<u32, u32> {
    use std::convert::TryInto;
    d.iter()
      .map(|(x, y)| ((*x).into(), y.len().try_into().unwrap()))
      .collect()
  }

  pub fn into_proto(self) -> proto::CompressedBlock {
    proto::CompressedBlock {
      lenlit_codelen: Self::dict_to_bitsused(self.lenlit_code),
      dist_codelen: Self::dict_to_bitsused(self.dist_code),
      data: self.data.into_iter().map(DeflateSym::into_proto).collect(),
    }
  }
}

impl BlockData {
  fn underlying(self) -> proto::underlying_block::Data {
    use proto::underlying_block::Data;
    match self {
      Self::Raw(block) => Data::Raw(block.into_proto()),
      Self::Fix(block) => Data::Block(block.into_proto()),
      Self::Dyn(block) => Data::Block(block.into_proto()),
    }
  }

  pub fn into_proto(self) -> proto::UnderlyingBlock {
    proto::UnderlyingBlock {
      data: Some(self.underlying()),
    }
  }
}

impl Block {
  pub fn into_proto(self) -> proto::DeflateBlock {
    proto::DeflateBlock {
      bfinal: self.bfinal,
      data: Some(self.data.into_proto()),
    }
  }
}

impl DeflateStream {
  pub fn into_proto(self) -> proto::DeflateStream {
    proto::DeflateStream {
      blocks: self.blocks.into_iter().map(Block::into_proto).collect(),
    }
  }
}

impl proto::Literal {
  pub fn validate(&self) -> String {
    if !(0..=256).contains(&self.value) {
      format!("Invalid Literal value {}", self.value)
    } else {
      String::new()
    }
  }
}

impl proto::Backref {
  pub fn validate(&self) -> String {
    let mut err = String::new();
    if !(0..=32768).contains(&self.distance) {
      err.push_str("Backref distance outside of valid range");
    }
    if !(0..=258).contains(&self.length) {
      err.push_str("Backref length outside of valid range");
    }
    err
  }
}

impl proto::OffsetBackref {
  pub fn validate(&self) -> String {
    unimplemented!()
  }
}

impl proto::DeflateSym {
  pub fn validate(&self) -> String {
    if let Some(x) = &self.sym {
      match x {
        proto::deflate_sym::Sym::Lit(x) => x.validate(),
        proto::deflate_sym::Sym::Backref(x) => x.validate(),
        proto::deflate_sym::Sym::Offset(x) => x.validate(),
      }
    } else {
      "DeflateSym enum not valid".to_owned()
    }
  }
}

impl proto::CompressedBlock {
  fn validate_lenlit_code(code: &std::collections::HashMap<u32, u32>) -> String {
    let mut log = String::new();
    for key in code.keys() {
      if !(0..=OFFSET_SIGIL).contains(&u16::try_from(*key).unwrap()) {
        log.push_str(&format!("Invalid lenlit code {}", &key));
      }
    }
    for val in code.values() {
      if *val > 16 {
        log.push_str(&format!("Invalid lenlit codelength {}", val));
      }
    }
    log
  }

  fn validate_dist_code(code: &std::collections::HashMap<u32, u32>) -> String {
    let mut log = String::new();
    for key in code.keys() {
      if !(MIN_DIST_CODE..=MAX_DIST_CODE).contains(&u16::try_from(*key).unwrap()) {
        log.push_str(&format!("Invalid lenlit code {}", &key));
      }
    }
    for val in code.values() {
      if *val > 16 {
        log.push_str(&format!("Invalid lenlit codelength {}", val));
      }
    }
    log
  }

  pub fn validate(&self) -> String {
    let a1 = Self::validate_lenlit_code(&self.lenlit_codelen);
    let a2 = Self::validate_dist_code(&self.dist_codelen);
    let a3 = self
      .data
      .iter()
      .map(proto::DeflateSym::validate)
      .filter(|x| !x.is_empty())
      .collect::<Vec<String>>()
      .join(";");

    let mut ret = String::new();
    if !a1.is_empty() {
      ret.push_str(&a1);
      ret.push(';');
    }
    if !a2.is_empty() {
      ret.push_str(&a2);
      ret.push(';');
    }
    if !a3.is_empty() {
      ret.push_str(&a3);
    }
    ret
  }
}

impl proto::UncompressedBlock {
  pub fn validate(&self) -> String {
    String::new()
  }
}

impl proto::UnderlyingBlock {
  pub fn validate(&self) -> String {
    if let Some(x) = &self.data {
      match x {
        proto::underlying_block::Data::Raw(x) => x.validate(),
        proto::underlying_block::Data::Block(x) => x.validate(),
      }
    } else {
      "UnderlyingBlock enum not valid".to_owned()
    }
  }
}

impl proto::DeflateBlock {
  pub fn validate(&self) -> String {
    if let Some(x) = &self.data {
      x.validate()
    } else {
      "DeflateBlock enum not valid".to_owned()
    }
  }
}

impl proto::DeflateStream {
  pub fn validate(&self) -> String {
    let nfinals = self.blocks.iter().map(|x| x.bfinal).filter(|x| *x).count();
    let mut errmsg = if nfinals != 1 {
      format!("Should only have one final block, but found {}", nfinals)
    } else {
      String::new()
    };
    let blockmsgs = self
      .blocks
      .iter()
      .map(proto::DeflateBlock::validate)
      .filter(|x| !x.is_empty())
      .collect::<Vec<String>>()
      .join("\n");

    errmsg.push_str(&blockmsgs);
    errmsg
  }
}
