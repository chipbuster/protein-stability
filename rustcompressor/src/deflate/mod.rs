/*! A straight implementation of the DEFLATE algorithm as specified in RFC
1951 (https://tools.ietf.org/html/rfc1951). Includes the reference
implementation of the compressor suggested at the end of the RFC. */

pub mod decoder;
pub mod encoder;
use bit_vec::BitVec;

/** Represents a compressed symbol in the DEFLATE stream: either a literal in
0-255 or <length, distance> pair.
*/
/* Implementor's note: These are not encoded naively, but instead
according to 3.2.5 of RFC 1951. Note that 3.2.5 only deals with the abstract
numbers needed to encode the codepoints: the actual binary representation of the
numbers is specified either according to 3.2.6 or the dynamic Huffman tree. */
pub enum CompSym {
  Literal(u8),
  /* A note about backreferences: the actual range of a backreference is [3,258]
  This *can* be captured in a u8, but special care is needed when trying to
  compute and store the resulting values */
  Backreference(u8, u16),
}

#[derive(Eq, PartialEq, Debug)]
pub enum BlockKind {
  Uncompressed,
  FixedCode,
  DynamicCode,
  Reserved,
}

#[derive(Debug)]
pub struct UncompressedBlock {
  data: Vec<u8>,
}

#[derive(Debug)]
pub struct FixedCodeBlock {
  data: BitVec,
}

#[derive(Debug)]
pub struct DynamicCodeBlock {
  hlit: u8,
  hdist: u8,
  hclen: u8,
  codelenlen: [u8; 19],
  codelenlit: Vec<u8>,
  codelendist: Vec<u8>,
  data: BitVec,
}

#[derive(Debug)]
pub enum BlockData {
  Raw(UncompressedBlock),
  Fix(FixedCodeBlock),
  Dyn(DynamicCodeBlock),
}

#[derive(Debug)]
pub struct Block {
  bfinal: bool,
  data: BlockData,
}

#[derive(Debug)]
pub struct DeflateStream {
  blocks: Vec<Block>,
}
