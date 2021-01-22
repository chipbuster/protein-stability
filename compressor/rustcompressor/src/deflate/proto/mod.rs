use crate::deflate::*;

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
  fn to_codelens(d: CodeDict<u16>) -> std::collections::HashMap<u32, u32> {
    use std::convert::TryInto;
    d.iter()
      .map(|(x, y)| ((*x).into(), y.len().try_into().unwrap()))
      .collect()
  }

  pub fn into_proto(self) -> proto::CompressedBlock {
    proto::CompressedBlock {
      lenlit_codelen: Self::to_codelens(self.lenlit_code),
      dist_codelen: Self::to_codelens(self.dist_code),
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
