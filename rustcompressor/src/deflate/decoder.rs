use super::default_data::default_codepoints::DecodeInfo;
use super::default_data::default_hufftree::default_read_hufftree;
use super::*;
use crate::huff_tree::{huffcode_from_lengths};

use std::collections::HashMap;
use std::io::Read;

use bitstream_io::{BitReader, LittleEndian};
use bitstream_io::huffman::compile_read_tree;

use thiserror::Error;
use lazy_static::lazy_static;

const BLOCK_END: u16 = 256;
const VERBOSE: bool = false;

macro_rules! debug_log {
  ($($arg:tt)*) => {
    if VERBOSE {
      print!($($arg)*)
    } else {
      print!("")
    }
  }
}

lazy_static! {
  pub static ref DEFAULT_READ_TREE: Box<[DeflateReadTree]> = default_read_hufftree();
  static ref DEFAULT_CODEPOINTS: DecodeInfo = DecodeInfo::new();
}

#[derive(Error, Debug)]
pub enum DeflateReadError {
  #[error("A reserved value was used in the bit")]
  ReservedValueUsed,
  #[error("Unexpected End of DEFLATE Data")]
  UnexpectedEndOfData,
  #[error("The input stream was not completely consumed")]
  StreamNotConsumed,
  #[error("Tried to go back {0} symbols, but the stream is only {1} large")]
  BackrefPastStart(u16, usize),
  #[error("Value out of range of valid encoded values: {0}")]
  CodeOutOfRange(u16),
  #[error("The LEN and NLEN fields of an uncompressed block mismatched: {0}, {1}")]
  LenNlenMismatch(u16, u16),
  #[error("Checksum mismatch: expected {:x} but data was {:x}", .0, .1)]
  ChecksumMismatch(u32, u32),
  #[error("Datasize mismatch: expected {0} but data was {1}")]
  DatasizeMismatch(u32, usize),
  #[error("Huffman Tree Construction Error")]
  HuffTreeError,
  #[error("Other IO error: {0}")]
  IOError(#[from] std::io::Error),
}

impl DeflateSym {
  // TODO: Implement functionality to read a symbol from bytestream, assuming
  // a particular hufftree encoding
  pub fn next_from_bitsource<R: Read>(
    bit_src: &mut BitReader<R, LittleEndian>,
    length_tree: &[DeflateReadTree],
    dist_tree: Option<&[DeflateReadTree]>,
  ) -> Result<Self, DeflateReadError> {
    let sym = bit_src.read_huffman(length_tree)?;
    if sym == BLOCK_END {
      debug_log!("End of block\n");
      Ok(Self::EndOfBlock)
    } else if sym <= 255 {
      debug_log!("Literal {}\n", sym);
      Ok(Self::Literal(sym as u8)) // Sym is within valid range for u8
    } else {
      let len_sym = sym;
      let len_codept = DEFAULT_CODEPOINTS.lookup_codept(len_sym);
      assert_eq!(len_codept.codept, len_sym);
      if len_codept.codept < 255 {
        println!("Got code {} when looking for a length", len_codept.codept);
        return Err(DeflateReadError::CodeOutOfRange(sym));
      }
      debug_log!("Got length code {} ", len_codept.codept);
      let len = len_codept.read_value_from_bitstream(bit_src)?;
      debug_log!("corresponding to match length {}\n", len);

      // Perform second read to get the distance. These are coded by 5 literal
      // bits, not using the huffman coding of the literal/length bits
      let dist_sym;
      if let Some(t) = dist_tree {
        dist_sym = bit_src.read_huffman(t)?;
      } else {
        debug_log!("Using raw bits for distance");
        // Read the bits in reverse order (for some reason??)
        let temp: u8 = bit_src.read(5)?;
        let mut out = 0u8;
        out |= (temp & 0b00001) << 4;
        out |= (temp & 0b00010) << 2;
        out |= temp & 0b00100;
        out |= (temp & 0b01000) >> 2;
        out |= (temp & 0b10000) >> 4;
        dist_sym = out as u16;
      }
      let dist_codept = DEFAULT_CODEPOINTS.lookup_codept(dist_sym);
      assert_eq!(dist_codept.codept, dist_sym);
      if dist_codept.codept > 29 {
        println!("Got code {} when looking for a dist", dist_codept.codept);
        return Err(DeflateReadError::CodeOutOfRange(sym));
      }
      debug_log!("Got distance code {} ", dist_codept.codept);
      let dist = dist_codept.read_value_from_bitstream(bit_src)?;
      debug_log!("corresponding to match distance {}\n", dist);

      Ok(Self::Backreference(len, dist)) // Sym is within valid range for u8
    }
  }
}

pub fn uncompressed_block_from_stream<R: Read>(
  bit_src: &mut BitReader<R, LittleEndian>,
) -> Result<UncompressedBlock, DeflateReadError> {
  debug_log!("Decompressing raw block\n");
  // According to 1951, we need to skip any remaining bits in the partial byte
  bit_src.byte_align();
  let mut byte_src = bit_src.bytereader().expect("Byte Align Failed");

  let mut u16bytes = [0u8; 2];
  byte_src.read_bytes(&mut u16bytes)?;
  let len = u16::from_le_bytes(u16bytes);
  byte_src.read_bytes(&mut u16bytes)?;
  let nlen = u16::from_le_bytes(u16bytes);

  if len != !nlen {
    return Err(DeflateReadError::LenNlenMismatch(len, nlen));
  }

  let mut payload = vec![0u8; len as usize];
  byte_src.read_bytes(&mut payload)?;

  assert_eq!(len, !nlen);
  assert_eq!(payload.len(), len as usize);

  let block = UncompressedBlock { data: payload };
  Ok(block)
}

fn compressed_block_from_stream<R: Read>(
  bit_src: &mut BitReader<R, LittleEndian>,
  length_tree: &[DeflateReadTree],
  dist_tree: Option<&[DeflateReadTree]>
) -> Result<CompressedBlock, DeflateReadError> {
  let mut contents = Vec::new();

  loop {
    let sym = DeflateSym::next_from_bitsource(bit_src, length_tree, dist_tree);
    match sym {
      Ok(DeflateSym::EndOfBlock) => return Ok(CompressedBlock { data: contents }),
      Ok(x) => contents.push(x),
      Err(e) => return Err(e),
    }
  }
}

fn fixed_block_from_stream<R: Read>(
  bit_src: &mut BitReader<R, LittleEndian>,
) -> Result<CompressedBlock, DeflateReadError> {
  debug_log!("Decompressing fixed block\n");
  compressed_block_from_stream(bit_src, &DEFAULT_READ_TREE, None)
}

/// Unpack the next set of code lengths
fn read_code_length_codeslength<R: Read>(
  bit_src: &mut BitReader<R, LittleEndian>,
  num_codes: u8,
) -> Result<Box<[DeflateReadTree]>, DeflateReadError> {
  let nc = num_codes as usize;
  let raw_code_order = vec![
    16u16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15,
  ];
  let mut codecodelen = HashMap::new();
  let codes = &raw_code_order[0..nc];
  for code in codes.into_iter() {
    let ccl: usize = bit_src.read::<u8>(3)? as usize;
    codecodelen.insert(*code, ccl);
  }

  let lens = huffcode_from_lengths(&codecodelen);

  match bitstream_io::huffman::compile_read_tree(lens){
    Ok(x) => Ok(x),
    Err(_) => Err(DeflateReadError::HuffTreeError),
  }
}

/// Read the bitstream and get codelengths for the literal/distance alphabet
/// in accordance with 3.2.7 of RFC 1951
fn decode_huffman_alphabets<R: Read>(
  bit_src: &mut BitReader<R, LittleEndian>,
  size_huffman: &[DeflateReadTree],
  num_literals: u16,
  num_distances: u16
) -> Result<(Box<[DeflateReadTree]>, Box<[DeflateReadTree]>), DeflateReadError> {
  let num_symbols = num_literals + num_distances;
  let mut lengths = Vec::new();
  let mut ctr = 0u16;
  while ctr < num_symbols{
    let s = bit_src.read_huffman(size_huffman)?;
    match s {
      0..=15 => {
        debug_log!("Adding codelength of length-{}\n", s);
        lengths.push(s);
        ctr += 1;
      }
      16 => {
        let r: u8 = bit_src.read(2)?;
        let repeat_length = 3 + r;
        let repeat_code = *lengths.last().unwrap();
        debug_log!("Adding {} codelengths of length-{}\n", repeat_length, repeat_code);
        for _ in 0..repeat_length{
          lengths.push(repeat_code);
        }
        ctr += repeat_length as u16;
      }
      17 => {
        let r: u8 = bit_src.read(3)?;
        let repeat_length = 3 + r;
        debug_log!("Adding {} 0-length codes\n", repeat_length);
        for _ in 0..repeat_length{
          lengths.push(0);
        }
        ctr += repeat_length as u16;
      }
      18 => {
        let r: u8 = bit_src.read(7)?;
        let repeat_length = 11 + r;
        debug_log!("Adding {} 0-length codes\n", repeat_length);
        for _ in 0..repeat_length{
          lengths.push(0);
        }
        ctr += repeat_length as u16;
      }
      _ => return Err(DeflateReadError::CodeOutOfRange(s))
    }
  }

  assert!(num_distances < num_literals);
  assert_eq!(lengths.len(), (num_literals + num_distances) as usize);
  // We now have the codelengths for distances and sizes. Get Huffman Code by
  // generating appropriate hashmaps;
  let mut literal_lengths = HashMap::<u16, usize>::new();
  let mut dist_lengths = HashMap::<u16, usize>::new();
  let max_literal = num_literals as usize - 1;
  // What the actual fuck am I doing here this is a monstrosity
  for (j, length) in lengths.into_iter().enumerate() {
    let symbol = j as u16;
    let symlen = length as usize;
    if j <= max_literal {
      literal_lengths.insert(symbol, symlen);
    } else {
      let symbol = j - max_literal - 1;
      let symbol = symbol as u16;
      dist_lengths.insert(symbol, symlen);
    }
  }
  debug_log!("{} literals and {} distance\n", literal_lengths.len(), dist_lengths.len());
  let litlen_code = huffcode_from_lengths(&literal_lengths);
  let dist_code = huffcode_from_lengths(&dist_lengths);

  let litlen_tree = match compile_read_tree(litlen_code){
    Ok(x) => x,
    Err(_) => return Err(DeflateReadError::HuffTreeError),
  };
  let dist_tree = match compile_read_tree(dist_code){
    Ok(x) => x,
    Err(_) => return Err(DeflateReadError::HuffTreeError),
  };

  Ok((litlen_tree, dist_tree))
}

fn dynamic_block_from_stream<R: Read>(
  bit_src: &mut BitReader<R, LittleEndian>,
) -> Result<CompressedBlock, DeflateReadError> {
  debug_log!("Decompressing dynamic block\n");
  // First, read appropriate values
  let hlit: u8 = bit_src.read(5)?;
  let hdist: u8 = bit_src.read(5)?;
  let hclen: u8 = bit_src.read(4)?;

  let size_codes = read_code_length_codeslength(bit_src, hclen + 4)?;

  let num_literals = 257 + hlit as u16;
  let num_dists = 1 + hdist as u16;

  println!("{}, {}", num_literals, num_dists);

  let (length_tree, dist_tree) = decode_huffman_alphabets(bit_src, &size_codes, num_literals, num_dists)?;

  compressed_block_from_stream(bit_src, &length_tree, Some(&dist_tree))
}

impl BlockData {
  pub fn new_from_compressed_stream<R: Read>(
    bit_src: &mut BitReader<R, LittleEndian>,
  ) -> Result<Self, DeflateReadError> {
    let btype = bit_src.read::<u8>(2)?;
    debug_log!("Block type is {}\n", btype);
    match btype {
      0b00 => Ok(Self::Raw(uncompressed_block_from_stream(bit_src)?)),
      0b01 => Ok(Self::Fix(fixed_block_from_stream(bit_src)?)),
      0b10 => Ok(Self::Dyn(dynamic_block_from_stream(bit_src)?)),
      0b11 => Err(DeflateReadError::ReservedValueUsed),
      _ => {
        println!("type = {}", btype);
        unreachable!("Got n >= 4 on a two-bit read. Programming error?")
      }
    }
  }
}

impl Block {
  pub fn new_from_compressed_stream<R: Read>(
    bit_src: &mut BitReader<R, LittleEndian>,
  ) -> Result<Self, DeflateReadError> {
    let final_block = bit_src.read_bit()?;
    debug_log!("Got final block bit {}\n", final_block);
    let data = BlockData::new_from_compressed_stream(bit_src)?;
    Ok(Block {
      bfinal: final_block,
      data,
    })
  }
}

impl DeflateStream {
  pub fn new_from_source<R: Read>(src: R) -> Result<Self, DeflateReadError> {
    let mut bit_src = BitReader::<R, LittleEndian>::new(src);
    let mut blocks = Vec::new();
    let mut has_more_blocks = true;
    while has_more_blocks {
      let res = Block::new_from_compressed_stream(&mut bit_src)?;
      has_more_blocks = !res.bfinal;
      blocks.push(res);
    }
    //Check: Have we actually consumed all the input?
    bit_src.byte_align();
    if let Ok(_) = bit_src.read_bit() {
      return Err(DeflateReadError::StreamNotConsumed);
    }
    Ok(Self { blocks })
  }

  // Expand a backref at the given
  fn expand_backref(
    length: u16,
    distance: u16,
    data: &mut Vec<u8>,
  ) -> Result<(), DeflateReadError> {
    let last_data_index = data.len();
    if distance as usize > last_data_index {
      return Err(DeflateReadError::BackrefPastStart(
        distance,
        last_data_index,
      ));
    }
    let first_i = last_data_index - distance as usize;
    if length > distance {
      let mut n = 0u16;
      loop {
        for j in first_i..last_data_index {
          let target = data[j];
          data.push(target);
          n += 1;
          if n == length {
            return Ok(());
          }
        }
      }
    } else {
      let last_i = first_i + length as usize - 1;
      for j in first_i..=last_i {
        let target = data[j];
        data.push(target);
      }
      return Ok(());
    }
  }

  /// Convert a stream of DEFLATE symbols into bytes by expanding backreferences
  pub fn into_byte_stream(self) -> Result<Vec<u8>, DeflateReadError> {
    let mut literals = Vec::<u8>::new();
    for block in self.blocks {
      debug_log!("{:?}\n", block);
      match block.data {
        BlockData::Raw(mut rawdata) => literals.append(&mut rawdata.data),
        BlockData::Fix(comp) | BlockData::Dyn(comp) => {
          for sym in comp.data.into_iter() {
            match sym {
              DeflateSym::Literal(ch) => literals.push(ch),
              DeflateSym::Backreference(length, distance) => {
                Self::expand_backref(length, distance, &mut literals)?;
              }
              DeflateSym::EndOfBlock => break,
            }
          }
        }
      }
    }
    Ok(literals)
  }

  /// Convert DEFLATE symbols into bytes, then check against checksum
  pub fn into_byte_stream_checked(self, crc32: u32, isz: u32) -> Result<Vec<u8>, DeflateReadError> {
    let literals = self.into_byte_stream()?;

    if literals.len() != isz as usize {
      return Err(DeflateReadError::DatasizeMismatch(isz, literals.len()));
    }
    let mut hasher = crc32fast::Hasher::new();
    hasher.update(&literals);
    let checksum = hasher.finalize();
    if checksum != crc32 {
      return Err(DeflateReadError::ChecksumMismatch(crc32, checksum));
    }
    debug_log!("Passed check with isz = {} and crc32 = {:x}\n", isz, crc32);
    Ok(literals)
  }
}

mod tests {
  #[allow(unused_imports)]
  use super::*;

  #[test]
  fn hello_uncompressed() {
    let data = [0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0xe7, 0x2, 0x0u8];
    let strm = DeflateStream::new_from_source(&data[..]).unwrap();
    let decoded = strm.into_byte_stream().unwrap();
    let correct_answer = [72, 101, 108, 108, 111, 10u8];
    assert_eq!(decoded, correct_answer);
  }
}
