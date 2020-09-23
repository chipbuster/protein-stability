/*! A straight implementation of the DEFLATE algorithm as specified in RFC
1951 (https://tools.ietf.org/html/rfc1951). Includes the reference
implementation of the compressor suggested at the end of the RFC. */

pub mod decoder;
mod default_data;
pub mod encoder;

use bitstream_io::huffman::{ReadHuffmanTree, WriteHuffmanTree};
use bitstream_io::LittleEndian;

type DeflateReadTree = ReadHuffmanTree<LittleEndian, u16>;
type DeflateWriteTree = WriteHuffmanTree<LittleEndian, u16>;

/** Represents a compressed symbol in the DEFLATE stream: either a literal in
0-255 or <length, distance> pair.
*/
/* Implementor's note: These are not encoded naively, but instead
according to 3.2.5 of RFC 1951. Note that 3.2.5 only deals with the abstract
numbers needed to encode the codepoints: the actual binary representation of the
numbers is specified either according to 3.2.6 or the dynamic Huffman tree. */
#[derive(Debug, PartialEq, Eq)]
pub enum DeflateSym {
  EndOfBlock,
  Literal(u8),
  Backreference(u16, u16),
}

#[derive(Debug)]
pub struct UncompressedBlock {
  data: Vec<u8>,
}

#[derive(Debug)]
pub struct CompressedBlock {
  data: Vec<DeflateSym>,
}

#[derive(Debug)]
pub enum BlockData {
  Raw(UncompressedBlock),
  Fix(CompressedBlock),
  Dyn(CompressedBlock),
}

#[derive(Debug)]
pub struct Block {
  pub bfinal: bool,
  pub data: BlockData,
}

#[derive(Debug)]
pub struct DeflateStream {
  pub blocks: Vec<Block>,
}
