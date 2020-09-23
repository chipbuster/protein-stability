use super::super::*;
use bitstream_io::{BitReader, LittleEndian};
use decoder::DeflateReadError;
use std::vec::Vec;

const LDSEP: usize = 30; // The point in DecodeInfo where we switch from
                         // length codes to distance codes.

#[derive(Debug, Copy, Clone)]
pub struct DecodeCodepoint {
  pub codept: u16,
  pub nbits: u8,
  pub lo: u16,
  pub hi: u16,
}

impl DecodeCodepoint {
  pub fn new(codept: u16, nbits: u8, lo: u16) -> Self {
    let range = if nbits == 0 { 0 } else { (1u16 << nbits) - 1 };
    let hi = lo + range;
    Self {
      codept,
      nbits,
      lo,
      hi,
    }
  }

  /// Read a value from bitstream using this codepoint, erroring if out of range
  pub fn read_value_from_bitstream<R: std::io::Read>(
    &self,
    bit_src: &mut BitReader<R, LittleEndian>,
  ) -> Result<u16, DeflateReadError> {
    let extra: u16 = bit_src.read(self.nbits as u32)?;
    let val = self.lo + extra;
    if val > self.hi {
      Err(DeflateReadError::CodeOutOfRange(val))
    } else {
      Ok(val)
    }
  }
}
pub struct DecodeInfo {
  pub info: Vec<DecodeCodepoint>,
}

impl DecodeInfo {
  pub fn new() -> DecodeInfo {
    let mut pts = Vec::new();

    // Distance Codes
    pts.push(DecodeCodepoint::new(0, 0, 1));
    pts.push(DecodeCodepoint::new(1, 0, 2));
    pts.push(DecodeCodepoint::new(2, 0, 3));
    pts.push(DecodeCodepoint::new(3, 0, 4));
    pts.push(DecodeCodepoint::new(4, 1, 5));
    pts.push(DecodeCodepoint::new(5, 1, 7));
    pts.push(DecodeCodepoint::new(6, 2, 9));
    pts.push(DecodeCodepoint::new(7, 2, 13));
    pts.push(DecodeCodepoint::new(8, 3, 17));
    pts.push(DecodeCodepoint::new(9, 3, 25));

    pts.push(DecodeCodepoint::new(10, 4, 33));
    pts.push(DecodeCodepoint::new(11, 4, 49));
    pts.push(DecodeCodepoint::new(12, 5, 65));
    pts.push(DecodeCodepoint::new(13, 5, 97));
    pts.push(DecodeCodepoint::new(14, 6, 129));
    pts.push(DecodeCodepoint::new(15, 6, 193));
    pts.push(DecodeCodepoint::new(16, 7, 257));
    pts.push(DecodeCodepoint::new(17, 7, 385));
    pts.push(DecodeCodepoint::new(18, 8, 513));
    pts.push(DecodeCodepoint::new(19, 8, 769));

    pts.push(DecodeCodepoint::new(20, 9, 1025));
    pts.push(DecodeCodepoint::new(21, 9, 1537));
    pts.push(DecodeCodepoint::new(22, 10, 2049));
    pts.push(DecodeCodepoint::new(23, 10, 3073));
    pts.push(DecodeCodepoint::new(24, 11, 4097));
    pts.push(DecodeCodepoint::new(25, 11, 6145));
    pts.push(DecodeCodepoint::new(26, 12, 8193));
    pts.push(DecodeCodepoint::new(27, 12, 12289));
    pts.push(DecodeCodepoint::new(28, 13, 16385));
    pts.push(DecodeCodepoint::new(29, 13, 24577));

    // Length Codes
    pts.push(DecodeCodepoint::new(257, 0, 3));
    pts.push(DecodeCodepoint::new(258, 0, 4));
    pts.push(DecodeCodepoint::new(259, 0, 5));
    pts.push(DecodeCodepoint::new(260, 0, 6));
    pts.push(DecodeCodepoint::new(261, 0, 7));
    pts.push(DecodeCodepoint::new(262, 0, 8));
    pts.push(DecodeCodepoint::new(263, 0, 9));
    pts.push(DecodeCodepoint::new(264, 0, 10));
    pts.push(DecodeCodepoint::new(265, 1, 11));
    pts.push(DecodeCodepoint::new(266, 1, 13));

    pts.push(DecodeCodepoint::new(267, 1, 15));
    pts.push(DecodeCodepoint::new(268, 1, 17));
    pts.push(DecodeCodepoint::new(269, 2, 19));
    pts.push(DecodeCodepoint::new(270, 2, 23));
    pts.push(DecodeCodepoint::new(271, 2, 27));
    pts.push(DecodeCodepoint::new(272, 2, 31));
    pts.push(DecodeCodepoint::new(273, 3, 35));
    pts.push(DecodeCodepoint::new(274, 3, 43));
    pts.push(DecodeCodepoint::new(275, 3, 51));
    pts.push(DecodeCodepoint::new(276, 3, 59));

    pts.push(DecodeCodepoint::new(277, 4, 67));
    pts.push(DecodeCodepoint::new(278, 4, 83));
    pts.push(DecodeCodepoint::new(279, 4, 99));
    pts.push(DecodeCodepoint::new(280, 4, 115));
    pts.push(DecodeCodepoint::new(281, 5, 131));
    pts.push(DecodeCodepoint::new(282, 5, 163));
    pts.push(DecodeCodepoint::new(283, 5, 195));
    pts.push(DecodeCodepoint::new(284, 5, 227));
    pts.push(DecodeCodepoint::new(285, 0, 258));

    Self { info: pts }
  }

  pub fn lookup_codept(&self, codept: u16) -> DecodeCodepoint {
    assert!(codept < 30 || (codept > 256 && codept < 286));
    if codept > 255 {
      let index = (30 + codept - 257) as usize;
      self.info[index]
    } else {
      let index = codept as usize;
      self.info[index]
    }
  }

  pub fn lookup_length(&self, length: u16) -> Option<DecodeCodepoint> {
    for c in &self.info[LDSEP..] {
      if length >= c.lo && length <= c.hi {
        return Some(*c);
      }
    }
    None
  }

  pub fn lookup_dist(&self, dist: u16) -> Option<DecodeCodepoint> {
    for c in &self.info[0..LDSEP] {
      if dist >= c.lo && dist <= c.hi {
        return Some(*c);
      }
    }
    None
  }
}