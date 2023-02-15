/*! A straight implementation of the DEFLATE algorithm as specified in RFC
1951 (https://tools.ietf.org/html/rfc1951). Includes the reference
implementation of the compressor suggested at the end of the RFC. */

pub mod decoder;
pub mod default_data;
pub mod encoder;
pub mod lz77;
pub mod proto;

mod codepoints;
mod deflate_header;

use bitstream_io::huffman::{ReadHuffmanTree, WriteHuffmanTree};
use bitstream_io::LittleEndian;

type DeflateReadTree = ReadHuffmanTree<LittleEndian, u16>;
type DeflateWriteTree = WriteHuffmanTree<LittleEndian, u16>;

/// An encoding dictionary, pre-compilation
type CodeDictEntry<S> = (S, Vec<u8>);
type CodeDict<S> = Vec<CodeDictEntry<S>>;

/** Represents a compressed symbol in the DEFLATE stream: either a literal in
0-255 or <length, distance> pair.
*/
/* Implementor's note: These are not encoded naively, but instead
according to 3.2.5 of RFC 1951. Note that 3.2.5 only deals with the abstract
numbers needed to encode the codepoints: the actual binary representation of the
numbers is specified either according to 3.2.6 or the dynamic Huffman tree. */
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LZSym<T> {
  EndOfBlock,
  Literal(u8),
  Backreference(T, T),
}

type DeflateSym = LZSym<u16>;

#[derive(Debug, Clone)]
pub struct UncompressedBlock {
  data: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct CompressedBlock {
  lenlit_code: CodeDict<u16>,
  dist_code: CodeDict<u16>,
  data: Vec<DeflateSym>,
}

#[derive(Debug, Clone)]
pub enum BlockData {
  Raw(UncompressedBlock),
  Fix(CompressedBlock),
  Dyn(CompressedBlock),
}

#[derive(Debug, Clone)]
pub struct Block {
  bfinal: bool,
  data: BlockData,
}

#[derive(Debug, Clone)]
pub struct DeflateStream {
  blocks: Vec<Block>,
}

#[derive(Debug, Clone)]
pub struct LZ77SymStream {
  pub symbols: Vec<LZSym<u64>>,
}

mod test {
  #[allow(unused_imports)]
  use super::*;

  #[cfg(test)]
  // Take some string of bytes and roundtrip it down to the bit level
  fn roundtrip_bitlevel_deflate(data: &Vec<u8>) -> Vec<u8> {
    let symbols = DeflateStream::new_from_raw_bytes_deflate(data);
    let sink = Vec::new();
    let encoded = symbols.write_to_bitstream(sink).unwrap();
    let decoded = DeflateStream::new_from_deflate_encoded_bits(&encoded[..]).unwrap();
    decoded.into_byte_stream().unwrap()
  }

  #[test]
  pub fn toplevel_roundtrip_1() {
    let data = "hellohellohelloIamGeronimohello".into();
    let rt_deflate = roundtrip_bitlevel_deflate(&data);
    assert_eq!(rt_deflate, data);
  }

  #[test]
  pub fn toplevel_roundtrip_2() {
    let data = "Entire any had depend and figure winter. Change stairs and men likely wisdom new happen piqued six. Now taken him timed sex world get. Enjoyed married an feeling delight pursuit as offered. As admire roused length likely played pretty to no. Means had joy miles her merry solid order.".into();
    let rt_deflate = roundtrip_bitlevel_deflate(&data);
    assert_eq!(rt_deflate, data);
  }

  #[test]
  pub fn toplevel_roundtrip_3() {
    let data = vec![
      0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    ];
    let rt_deflate = roundtrip_bitlevel_deflate(&data);
    assert_eq!(rt_deflate, data);
  }

  #[test]
  pub fn toplevel_roundtrip_4() {
    let mut data = vec![1, 2, 3, 4, 3, 2, 1];
    for i in 0..10 {
      data.push(i);
    }
    data.append(&mut vec![15, 16, 17, 18, 17, 16, 15]);
    let rt_deflate = roundtrip_bitlevel_deflate(&data);
    assert_eq!(rt_deflate, data);
  }
}
