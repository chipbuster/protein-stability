use super::*;
use bit_vec::BitVec;
use bitstream_io::huffman::{compile_read_tree, HuffmanTreeError};
use bitstream_io::{BitReader, Endianness, LittleEndian};
use std::io::Read;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum DeflateReadError {
  #[error("A reserved value was used in the bit")]
  ReservedValueUsed,
  #[error("Unexpected End of DEFLATE Data")]
  UnexpectedEndOfData,
  #[error("The LEN and NLEN fields of an uncompressed block mismatched: {0}, {1}")]
  LenNlenMismatch(u16, u16),
  #[error("Huffman Tree Generation Error: {0}")]
  HuffTreeError(#[from] HuffmanTreeError),
  #[error("Other IO error: {0}")]
  IOError(#[from] std::io::Error),
}

// A utility function
fn read_n<R: Read, E: Endianness>(
  src: &mut BitReader<R, E>,
  mut n: usize,
) -> Result<u8, DeflateReadError> {
  assert!(n <= 8);
  let mut x = 0u8;
  while n > 0 {
    let bit = src.read_bit()? as u8;
    x = x << 1;
    x = x | bit;
    n -= 1;
  }
  Ok(x)
}

// TODO: Implement default hufftree (for fixed blocks)

impl CompSym {
  // TODO: Implement functionality to read a symbol from bytestream, assuming
  // a particular hufftree encoding
}

impl UncompressedBlock {
  pub fn new_from_compressed_stream<R: Read>(
    bit_src: &mut BitReader<R, LittleEndian>,
  ) -> Result<Self, DeflateReadError> {
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

    let block = Self { data: payload };
    Ok(block)
  }
}

impl FixedCodeBlock {
  pub fn new_from_compressed_stream<R: Read>(
    bit_src: &mut BitReader<R, LittleEndian>,
  ) -> Result<Self, DeflateReadError> {
    unimplemented!("")
  }
}

impl DynamicCodeBlock {
  pub fn new_from_compressed_stream<R: Read>(
    bit_src: &mut BitReader<R, LittleEndian>,
  ) -> Result<Self, DeflateReadError> {
    unimplemented!("")
  }
}

impl BlockData {
  pub fn new_from_compressed_stream<R: Read>(
    bit_src: &mut BitReader<R, LittleEndian>,
  ) -> Result<Self, DeflateReadError> {
    let btype = read_n(bit_src, 2)?;

    match btype {
      0b00 => Ok(Self::Raw(UncompressedBlock::new_from_compressed_stream(
        bit_src,
      )?)),
      0b00 => Ok(Self::Fix(FixedCodeBlock::new_from_compressed_stream(
        bit_src,
      )?)),
      0b00 => Ok(Self::Dyn(DynamicCodeBlock::new_from_compressed_stream(
        bit_src,
      )?)),
      0b11 => Err(DeflateReadError::ReservedValueUsed),
      _ => unreachable!("Got n >= 4 on a two-bit read. Programming error?"),
    }
  }
}

impl Block {
  pub fn new_from_compressed_stream<R: Read>(
    bit_src: &mut BitReader<R, LittleEndian>,
  ) -> Result<Self, DeflateReadError> {
    let fin = bit_src.read_bit()?;
    let data = BlockData::new_from_compressed_stream(bit_src)?;
    Ok(Block { bfinal: fin, data })
  }
}
