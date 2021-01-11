use super::codepoints::DEFAULT_CODEPOINTS;
use super::default_data::default_huffcode::*;
use super::deflate_header::*;
use super::*;

use std::io::Read;

use bitstream_io::huffman::compile_read_tree;
use bitstream_io::{BitReader, LittleEndian};

use thiserror::Error;

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

#[derive(Error, Debug)]
pub enum DeflateReadError {
  #[error("A reserved value was used in the bit")]
  ReservedValueUsed,
  #[error("Unexpected End of DEFLATE Data")]
  UnexpectedEndOfData,
  #[error("The input stream was not completely consumed")]
  StreamNotConsumed,
  #[error("Attempted to decode an offset DEFLATE block with offsets turned off")]
  UnexpectedOffsetSigil,
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
  #[error("Other IO error: {0}")]
  IOError(#[from] std::io::Error),
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
  length_codes: CodeDict<u16>,
  dist_codes: CodeDict<u16>,
  use_offset: bool,
) -> Result<CompressedBlock, DeflateReadError> {
  let mut contents = Vec::new();

  let length_tree = compile_read_tree_local(length_codes.clone())
    .expect("Empty length/literal dictionary. Programming bug?");
  // If dist_codes are empty (i.e. they were not provided), we use the default
  // tree.
  let dist_tree = if dist_codes.is_empty() {
    compile_read_tree_local(DEFAULT_DIST_CODE.clone())
  } else {
    compile_read_tree_local(dist_codes.clone())
  }
  .expect("Could not compile a dist tree");

  loop {
    let sym = DEFAULT_CODEPOINTS.read_sym(
      bit_src,
      length_tree.as_ref(),
      dist_tree.as_ref(),
      None,
      use_offset,
    );
    match sym {
      Ok(DeflateSym::EndOfBlock) => {
        return Ok(CompressedBlock {
          lenlit_code: length_codes,
          dist_code: dist_codes,
          data: contents,
        })
      }
      Ok(x) => contents.push(x),
      Err(e) => return Err(e),
    }
  }
}

/// Provides a wrapper around read tree compilation to deal with common issues.
// We could return a result in this case, but since Huffman doesn't implement
// error, all we can do is return a DeflateReadError::HuffTreeError. Instead,
// just return an option--None indicates a failure in tree compilation.
fn compile_read_tree_local(mut dict: CodeDict<u16>) -> Option<Box<[DeflateReadTree]>> {
  if dict.is_empty() {
    return None;
  }
  /* Thanks to some faffery in the bitstream-io library, we can't compile
  singleton trees. If we encounter this, we know that the only symbol in the
  dict has to be zero (due to how canonical huffman coding works), so we can
  specify another symbol to be encoded as 1, even though it will never be read */
  if dict.len() == 1 {
    let (x, y) = &dict[0];
    assert!(
      y.len() == 1 && y[0] == 0,
      "Invalid canonical code for singleton"
    );
    if *x == 0 {
      dict.push((1, vec![1]));
    } else {
      dict.push((0, vec![1]));
    }
  }

  match compile_read_tree(dict) {
    Ok(x) => Some(x),
    Err(e) => {
      println!("WARN: Got {:?} when compiling tree. Probably a bad sign", e);
      None
    }
  }
}

/// Reads a DEFLATE block encoded with the default trees out of the given bitstream
fn fixed_block_from_stream<R: Read>(
  bit_src: &mut BitReader<R, LittleEndian>,
) -> Result<CompressedBlock, DeflateReadError> {
  debug_log!("Decompressing fixed block\n");
  compressed_block_from_stream(
    bit_src,
    DEFAULT_LENGTH_CODE.clone(),
    DEFAULT_DIST_CODE.clone(),
    false,
  )
}

/// Reads a DEFLATE block encoded with custom trees out of the given bitstream
fn dynamic_block_from_stream<R: Read>(
  bit_src: &mut BitReader<R, LittleEndian>,
) -> Result<CompressedBlock, DeflateReadError> {
  debug_log!("Decompressing dynamic block\n");
  let (length_dict, dist_dict) = read_header(bit_src)?;

  compressed_block_from_stream(bit_src, length_dict, dist_dict, false)
}

/// Reads a DEFLATE block encoded with custom trees and potentially using OFFSET
/// encoding out of the given bitstream
fn offset_block_from_stream<R: Read>(
  bit_src: &mut BitReader<R, LittleEndian>,
) -> Result<CompressedBlock, DeflateReadError> {
  debug_log!("Decompressing offset block\n");
  let (length_dict, dist_dict) = read_header(bit_src)?;

  compressed_block_from_stream(bit_src, length_dict, dist_dict, true)
}

impl BlockData {
  pub fn new_from_compressed_stream<R: Read>(
    bit_src: &mut BitReader<R, LittleEndian>,
    use_offset: bool,
  ) -> Result<Self, DeflateReadError> {
    let btype = bit_src.read::<u8>(2)?;
    debug_log!("Block type is {}\n", btype);
    if use_offset {
      assert!(
        btype == 0b10,
        "Requested to decode with offset-backreferences, but block is not dynamically encoded!"
      );
    }

    match btype {
      0b00 => Ok(Self::Raw(uncompressed_block_from_stream(bit_src)?)),
      0b01 => Ok(Self::Fix(fixed_block_from_stream(bit_src)?)),
      0b10 => {
        if use_offset {
          Ok(Self::Dyn(offset_block_from_stream(bit_src)?))
        } else {
          Ok(Self::Dyn(dynamic_block_from_stream(bit_src)?))
        }
      }
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
    use_offset: bool,
  ) -> Result<Self, DeflateReadError> {
    let final_block = bit_src.read_bit()?;
    debug_log!("Got final block bit {}\n", final_block);
    let data = BlockData::new_from_compressed_stream(bit_src, use_offset)?;
    Ok(Block {
      bfinal: final_block,
      data,
    })
  }
}

impl CompressedBlock {
  /// Expand a backref at the given point in the data.
  fn expand_backref(
    length: u16,
    distance: u16,
    data: &mut Vec<u8>,
    offset: u8,
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
          data.push(target + offset);
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
        data.push(target + offset);
      }
      return Ok(());
    }
  }

  /** Decode this compressed block into a stream of bytes. Because blocks can
      refer to data in earlier blocks, requires caller to pass in (potentially
      empty) vector of already-decoded data from prior blocks.
  */
  pub fn into_decompressed_bytes(self, decoded: &mut Vec<u8>) -> Result<(), DeflateReadError> {
    for sym in self.data.into_iter() {
      match sym {
        DeflateSym::Literal(ch) => decoded.push(ch),
        DeflateSym::Backreference(length, distance) => {
          Self::expand_backref(length, distance, decoded, 0)?;
        }
        DeflateSym::OffsetBackref(offset, length, distance) => {
          Self::expand_backref(length, distance, decoded, offset)?;
        }
        DeflateSym::EndOfBlock => break,
      }
    }
    Ok(())
  }
}

impl DeflateStream {
  pub fn new_from_offset_encoded_bits<R: Read>(src: R) -> Result<Self, DeflateReadError> {
    Self::new_from_encoded_bits(src, true)
  }
  pub fn new_from_deflate_encoded_bits<R: Read>(src: R) -> Result<Self, DeflateReadError> {
    Self::new_from_encoded_bits(src, false)
  }

  fn new_from_encoded_bits<R: Read>(src: R, use_offset: bool) -> Result<Self, DeflateReadError> {
    let mut bit_src = BitReader::<R, LittleEndian>::new(src);
    let mut blocks = Vec::new();
    let mut has_more_blocks = true;
    while has_more_blocks {
      let res = Block::new_from_compressed_stream(&mut bit_src, use_offset)?;
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

  /// Convert a stream of DEFLATE symbols into bytes by expanding backreferences
  pub fn into_byte_stream(self) -> Result<Vec<u8>, DeflateReadError> {
    let mut literals = Vec::<u8>::new();
    for block in self.blocks {
      debug_log!("{:?}\n", block);
      match block.data {
        BlockData::Raw(mut rawdata) => literals.append(&mut rawdata.data),
        BlockData::Fix(comp) | BlockData::Dyn(comp) => {
          comp.into_decompressed_bytes(&mut literals)?;
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
    let strm = DeflateStream::new_from_deflate_encoded_bits(&data[..]).unwrap();
    let decoded = strm.into_byte_stream().unwrap();
    let correct_answer = [72, 101, 108, 108, 111, 10u8];
    assert_eq!(decoded, correct_answer);
  }

  #[test]
  fn hello_dyn_compressed() {
    let data = [
      0xcb, 0x48, 0xcd, 0xc9, 0xc9, 0xcf, 0x80, 0x13, 0x5c, 0x19, 0xa3, 0x7c, 0xaa, 0xf2, 0x01,
    ];
    let strm = DeflateStream::new_from_deflate_encoded_bits(&data[..]).unwrap();
    let decoded = strm.into_byte_stream().unwrap();
    let correct_answer_string = "hellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\nhellohellohello\n";
    let correct_answer = correct_answer_string.as_bytes();

    assert_eq!(decoded, correct_answer)
  }
}
