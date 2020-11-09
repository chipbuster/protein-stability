/*! Codepoints are used by RFC 1951 to encode distances and lengths. Each codepoint
consists of a value, encoded using the appropriate Huffman Tree, possibly followed
by a number of literal bits that are used to disambiguate the codepoint.

This module unifies the encoding and decoding of codepoints, which were
previously handled separately (leading to a fair amount of code duplication). */

/* The code table from RFC 1951 is reproduced below to avoid extra ALT-TABs

             Extra               Extra               Extra
        Code Bits Length(s) Code Bits Lengths   Code Bits Length(s)
        ---- ---- ------     ---- ---- -------   ---- ---- -------
         257   0     3       267   1   15,16     277   4   67-82
         258   0     4       268   1   17,18     278   4   83-98
         259   0     5       269   2   19-22     279   4   99-114
         260   0     6       270   2   23-26     280   4  115-130
         261   0     7       271   2   27-30     281   5  131-162
         262   0     8       272   2   31-34     282   5  163-194
         263   0     9       273   3   35-42     283   5  195-226
         264   0    10       274   3   43-50     284   5  227-257
         265   1  11,12      275   3   51-58     285   0    258
         266   1  13,14      276   3   59-66

              Extra           Extra               Extra
         Code Bits Dist  Code Bits   Dist     Code Bits Distance
         ---- ---- ----  ---- ----  ------    ---- ---- --------
           0   0    1     10   4     33-48    20    9   1025-1536
           1   0    2     11   4     49-64    21    9   1537-2048
           2   0    3     12   5     65-96    22   10   2049-3072
           3   0    4     13   5     97-128   23   10   3073-4096
           4   1   5,6    14   6    129-192   24   11   4097-6144
           5   1   7,8    15   6    193-256   25   11   6145-8192
           6   2   9-12   16   7    257-384   26   12  8193-12288
           7   2  13-16   17   7    385-512   27   12 12289-16384
           8   3  17-24   18   8    513-768   28   13 16385-24576
           9   3  25-32   19   8   769-1024   29   13 24577-32768

    Additionally, the offset-backreference encoder uses length codepoint
    286 to indicate the presence of an offset-backreference. (The maximum
    length codepoint as restricted by the max size of HLIT for specifying codes
    is 287.)

    The format for this type of backreference is:

    286 (encoded with the length/literal tree)
    Literal offset (encoded with the length/literal tree)
    Length (as encoded by a codepoint)
    Distance (as encoded by a codepoint)
*/

use bitstream_io::{BitReader, BitWriter, LittleEndian};
use lazy_static::lazy_static;
use std::io::{Read, Write};
use std::vec::Vec;
use std::convert::TryInto;

use super::{DeflateReadTree, DeflateSym, DeflateWriteTree};
use crate::deflate::Block;
use crate::deflate::decoder::DeflateReadError;
use crate::deflate::encoder::DeflateWriteError;

pub const MIN_DIST_CODE: u16 = 0;
pub const MAX_DIST_CODE: u16 = 29;
pub const MIN_LENGTH_CODE: u16 = 257;
pub const MAX_LENGTH_CODE: u16 = 285;
pub const OFFSET_SIGIL: u16 = 286;
pub const EOF_CODE: u16 = 256;

lazy_static! {
  pub static ref DEFAULT_CODEPOINTS: CodepointEncoder = CodepointEncoder::new();
}

///Asdasd
#[derive(Debug, Copy, Clone)]
pub struct Codepoint {
  code: u16,
  nbits: u8,
  lo: u16,
  hi: u16,
}

impl Codepoint {
  pub fn code(&self) -> u16 {
    self.code
  }
}

// Reverse the 5 LSBs of the given input. Utility for tree-free bitreading.
fn bitreverse5(data: u16) -> u16 {
  let mut out = 0u16;
  out |= (data & 0b00001) << 4;
  out |= (data & 0b00010) << 2;
  out |= data & 0b00100;
  out |= (data & 0b01000) >> 2;
  out |= (data & 0b10000) >> 4;
  out
}

impl Codepoint {
  pub fn new(code: u16, nbits: u8, lo: u16) -> Self {
    let range = if nbits == 0 { 0 } else { (1u16 << nbits) - 1 };
    let hi = lo + range;
    Self {
      code,
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

pub struct CodepointEncoder {
  length_codepoints: Vec<Codepoint>,
  dist_codepoints: Vec<Codepoint>,
  offset_codepoint: Codepoint,
}

impl CodepointEncoder {
  pub fn new() -> Self {
    let mut dist_pts = Vec::new();

    dist_pts.push(Codepoint::new(0, 0, 1));
    dist_pts.push(Codepoint::new(1, 0, 2));
    dist_pts.push(Codepoint::new(2, 0, 3));
    dist_pts.push(Codepoint::new(3, 0, 4));
    dist_pts.push(Codepoint::new(4, 1, 5));
    dist_pts.push(Codepoint::new(5, 1, 7));
    dist_pts.push(Codepoint::new(6, 2, 9));
    dist_pts.push(Codepoint::new(7, 2, 13));
    dist_pts.push(Codepoint::new(8, 3, 17));
    dist_pts.push(Codepoint::new(9, 3, 25));

    dist_pts.push(Codepoint::new(10, 4, 33));
    dist_pts.push(Codepoint::new(11, 4, 49));
    dist_pts.push(Codepoint::new(12, 5, 65));
    dist_pts.push(Codepoint::new(13, 5, 97));
    dist_pts.push(Codepoint::new(14, 6, 129));
    dist_pts.push(Codepoint::new(15, 6, 193));
    dist_pts.push(Codepoint::new(16, 7, 257));
    dist_pts.push(Codepoint::new(17, 7, 385));
    dist_pts.push(Codepoint::new(18, 8, 513));
    dist_pts.push(Codepoint::new(19, 8, 769));

    dist_pts.push(Codepoint::new(20, 9, 1025));
    dist_pts.push(Codepoint::new(21, 9, 1537));
    dist_pts.push(Codepoint::new(22, 10, 2049));
    dist_pts.push(Codepoint::new(23, 10, 3073));
    dist_pts.push(Codepoint::new(24, 11, 4097));
    dist_pts.push(Codepoint::new(25, 11, 6145));
    dist_pts.push(Codepoint::new(26, 12, 8193));
    dist_pts.push(Codepoint::new(27, 12, 12289));
    dist_pts.push(Codepoint::new(28, 13, 16385));
    dist_pts.push(Codepoint::new(29, 13, 24577));

    let mut len_pts = Vec::new();
    len_pts.push(Codepoint::new(257, 0, 3));
    len_pts.push(Codepoint::new(258, 0, 4));
    len_pts.push(Codepoint::new(259, 0, 5));
    len_pts.push(Codepoint::new(260, 0, 6));
    len_pts.push(Codepoint::new(261, 0, 7));
    len_pts.push(Codepoint::new(262, 0, 8));
    len_pts.push(Codepoint::new(263, 0, 9));
    len_pts.push(Codepoint::new(264, 0, 10));
    len_pts.push(Codepoint::new(265, 1, 11));
    len_pts.push(Codepoint::new(266, 1, 13));

    len_pts.push(Codepoint::new(267, 1, 15));
    len_pts.push(Codepoint::new(268, 1, 17));
    len_pts.push(Codepoint::new(269, 2, 19));
    len_pts.push(Codepoint::new(270, 2, 23));
    len_pts.push(Codepoint::new(271, 2, 27));
    len_pts.push(Codepoint::new(272, 2, 31));
    len_pts.push(Codepoint::new(273, 3, 35));
    len_pts.push(Codepoint::new(274, 3, 43));
    len_pts.push(Codepoint::new(275, 3, 51));
    len_pts.push(Codepoint::new(276, 3, 59));

    len_pts.push(Codepoint::new(277, 4, 67));
    len_pts.push(Codepoint::new(278, 4, 83));
    len_pts.push(Codepoint::new(279, 4, 99));
    len_pts.push(Codepoint::new(280, 4, 115));
    len_pts.push(Codepoint::new(281, 5, 131));
    len_pts.push(Codepoint::new(282, 5, 163));
    len_pts.push(Codepoint::new(283, 5, 195));
    len_pts.push(Codepoint::new(284, 5, 227));
    len_pts.push(Codepoint::new(285, 0, 258));

    let offsetcp = Codepoint::new(259, 0, 259);

    Self {
      length_codepoints: len_pts,
      dist_codepoints: dist_pts,
      offset_codepoint: offsetcp,
    }
  }

  fn get_codepoint_for_code(&self, code: u16) -> Codepoint {
    if code > MIN_DIST_CODE && code < MAX_DIST_CODE {
      self.dist_codepoints[code as usize]
    } else if code > MIN_LENGTH_CODE && code < MAX_LENGTH_CODE {
      self.length_codepoints[(code - MIN_LENGTH_CODE) as usize]
    } else if code == OFFSET_SIGIL {
      self.offset_codepoint
    } else {
      panic!("Invalid code passed to get_codepoint: {}", code);
    }
  }

  /// Given a length to encode, find the appropriate codepoint for it.
  pub fn get_codepoint_for_length(&self, val: u16) -> Codepoint {
    for pt in self.length_codepoints.iter() {
      if val >= pt.lo && val <= pt.hi {
        return *pt;
      }
    }
    panic!("No codepoint found for length {}, why?", val);
  }

  /// Given a distance to encode, find the appropriate codepoint for it.
  pub fn get_codepoint_for_dist(&self, val: u16) -> Codepoint {
    for pt in self.dist_codepoints.iter() {
      if val >= pt.lo && val <= pt.hi {
        return *pt;
      }
    }
    panic!("No codepoint found for distance {}, why?", val);
  }

  /// Encode a length to the given bitstream by writing its code, then its extra bits
  pub fn write_length<W: Write>(
    &self,
    length: u16,
    length_tree: &DeflateWriteTree,
    bit_sink: &mut BitWriter<W, LittleEndian>,
  ) -> Result<(), DeflateWriteError> {
    let c = &DEFAULT_CODEPOINTS.get_codepoint_for_length(length);
    let extra_bits = length - c.lo;
    bit_sink.write_huffman(length_tree, c.code)?;
    bit_sink.write(c.nbits as u32, extra_bits)?;
    Ok(())
  }

  pub fn write_dist<W: Write>(
    &self,
    dist: u16,
    dist_tree: Option<&DeflateWriteTree>,
    bit_sink: &mut BitWriter<W, LittleEndian>,
  ) -> Result<(), DeflateWriteError> {
    let c = &DEFAULT_CODEPOINTS.get_codepoint_for_dist(dist);
    let extra_bits = dist - c.lo;
    if let Some(d) = dist_tree {
      bit_sink.write_huffman(d, c.code)?;
    } else {
      let out = bitreverse5(c.code);
      bit_sink.write(5, out)?;
    }
    bit_sink.write(c.nbits as u32, extra_bits)?;

    Ok(())
  }

  pub fn write_offset<W: Write>(
    &self,
    offset: u8,
    tree: &DeflateWriteTree,
    bit_sink: &mut BitWriter<W, LittleEndian>,
  ) -> Result<(), DeflateWriteError> {
    // We could look up the codepoint for this, but it's a formality.
    bit_sink.write_huffman(tree, OFFSET_SIGIL)?;
    bit_sink.write_huffman(tree, offset as u16)?;
    Ok(())
  }

  pub fn read_sym<R: Read>(
    &self,
    bit_src: &mut BitReader<R, LittleEndian>,
    length_tree: &[DeflateReadTree],
    dist_tree: Option<&[DeflateReadTree]>,
    code: Option<u16>,
    use_offset: bool,
  ) -> Result<DeflateSym, DeflateReadError> {
    let code = match code {
      None => bit_src.read_huffman(length_tree)?,
      Some(x) => x,
    };
    println!("Got code {}", code);
    match code {
      0..=255 => Ok(DeflateSym::Literal(code as u8)),
      256 => Ok(DeflateSym::EndOfBlock),
      257..=285 => {
        let length = self.read_length(bit_src, length_tree, Some(code))?;
        let dist = self.read_dist(bit_src, dist_tree, None)?;
        println!("Read backreference {}, {}", length, dist);
        Ok(DeflateSym::Backreference(length, dist))
      }
      OFFSET_SIGIL => {
        if use_offset {
          let offset = self.read_offset(bit_src, length_tree, Some(code))?;
          let length = self.read_length(bit_src, length_tree, None)?;
          let dist = self.read_dist(bit_src, dist_tree, None)?;
          let ofs: u8 = offset.try_into().expect("Offset does not fit in u8");
          Ok(DeflateSym::OffsetBackref(ofs, length, dist))
        } else {
          Err(DeflateReadError::UnexpectedOffsetSigil)
        }
      }
      too_high => Err(DeflateReadError::CodeOutOfRange(too_high)),
    }
  }

  /* The following three functions read the appropriate type of codepoint-based
  info out of the provided bitstream. In some cases, it is impossible to call
  this function without already having read the first code out of the stream--in
  these cases, the code argument is provided to indicate that the first code
  has already been partially read. If the argument is None, it is assumed that
  no data from this sym has been read yet. */

  fn read_offset<R: Read>(
    &self,
    bit_src: &mut BitReader<R, LittleEndian>,
    length_tree: &[DeflateReadTree],
    code: Option<u16>,
  ) -> Result<u16, DeflateReadError> {
    if let Some(val) = code {
      assert_eq!(val, OFFSET_SIGIL, "Attempted to read offset with non-offset code.");
    } else {
      let val = bit_src.read_huffman(length_tree)?;
      assert_eq!(val, OFFSET_SIGIL, "Attempted to read offset with non-offset code.");
    };
    // Need to read offset here 
    unimplemented!("Cannot read offsets yet due to code regression");
  }

  fn read_length<R: Read>(
    &self,
    bit_src: &mut BitReader<R, LittleEndian>,
    length_tree: &[DeflateReadTree],
    code: Option<u16>,
  ) -> Result<u16, DeflateReadError> {
    let val = match code {
      None => bit_src.read_huffman(length_tree)?,
      Some(x) => x
    };
    assert!(val > 256, "Attempted to read length with invalid code {}",val);
    let len_index: usize = (val - MIN_LENGTH_CODE).try_into().unwrap();
    let codept = self.length_codepoints[len_index];
    let length_val = codept.read_value_from_bitstream(bit_src)?;
    Ok(length_val)
  }

  fn read_dist<R: Read>(
    &self,
    bit_src: &mut BitReader<R, LittleEndian>,
    dist_tree: Option<&[DeflateReadTree]>,
    code: Option<u16>,
  ) -> Result<u16, DeflateReadError> {
    let val = if let Some(c) = code {
      c
    } else if let Some(tree) = dist_tree {
        bit_src.read_huffman(tree)?
    } else {
        let revcode = bit_src.read(5)?;
        bitreverse5(revcode)
    };
    assert!(val < 30, "Attempted to read dist with invalid code {}",val);

    let dist_index: usize = val.try_into().unwrap();
    let codept = self.dist_codepoints[dist_index];
    let dist_val = codept.read_value_from_bitstream(bit_src)?;
    Ok(dist_val)
  }
}
