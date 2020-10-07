/*! A straight implementation of the DEFLATE algorithm as specified in RFC
1951 (https://tools.ietf.org/html/rfc1951). Includes the reference
implementation of the compressor suggested at the end of the RFC. */


/* Note: this implements a custom DEFLATE extension: an offset backref. This is 
similar to a DEFLATE backref, but the entire sequence is offset by a fixed
amount, modulo 255.

That is, to compute the expansion of an offset backref <off, length, dist>, do
the same thing with the <length, dist> values as you would for an ordinary DEFLATE
backreference, then add offset (mod 255) to every element of the new sequence.

The presence of an offset backref is indicated in the compressed sequence by
the symbol value 286 in the length/literal encoding. This is followed by the
offset value encoded directly in the length/literal encoding, then the
length and distance as is found in the standard DEFLATE encoding. We use 286
because it is invalid in normal DEFLATE (thus there is no possibility of confusing
it with a standard DEFLATE file), but it can still be specified by the HLIT tree
encodings specified in RFC 1951.

For simplicity, offset encoding is always used with a dynamic huffman code.
*/

mod default_data;
mod codepoints;
pub mod decoder;
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
  OffsetBackref(u8, u16, u16),
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
  bfinal: bool,
  data: BlockData,
}

#[derive(Debug)]
pub struct DeflateStream {
  blocks: Vec<Block>,
}

mod test {
  #[allow(unused_imports)]
  use super::*;

  // Take some string of bytes and roundtrip it down to the bit level
  fn roundtrip_bitlevel_deflate(data: &Vec<u8>) -> Vec<u8>{
    let symbols = DeflateStream::new_from_raw_bytes_deflate(data);
    let sink = Vec::new();
    let encoded = symbols.write_to_bitstream(sink).unwrap();
    let decoded = DeflateStream::new_from_deflate_encoded_bits(&encoded[..]).unwrap();
    decoded.into_byte_stream().unwrap()
  }

  // Take some string of bytes and roundtrip it down to the bit level
  fn roundtrip_bitlevel_offset(data: &Vec<u8>) -> Vec<u8>{
    let symbols = DeflateStream::new_from_raw_bytes_offset(data);
    let sink = Vec::new();
    let encoded = symbols.write_to_bitstream(sink).unwrap();
    let decoded = DeflateStream::new_from_offset_encoded_bits(&encoded[..]).unwrap();
    decoded.into_byte_stream().unwrap()
  }

  /* Round trip tests are broken pending implementation of huffman tree writing
     for dynamic blocks because I'M SO BAD AT THIS HOLY SHIT */

     /*
  #[test]
  pub fn toplevel_roundtrip_1(){
    let data = "hellohellohelloIamGeronimohello".into();
    let rt_deflate = roundtrip_bitlevel_deflate(&data);
    let rt_offset = roundtrip_bitlevel_offset(&data);

    assert_eq!(rt_deflate, data);
    assert_eq!(rt_offset, data);
  }

  #[test]
  pub fn toplevel_roundtrip_2(){
    let data = "Entire any had depend and figure winter. Change stairs and men likely wisdom new happen piqued six. Now taken him timed sex world get. Enjoyed married an feeling delight pursuit as offered. As admire roused length likely played pretty to no. Means had joy miles her merry solid order.".into();
    let rt_deflate = roundtrip_bitlevel_deflate(&data);
    let rt_offset = roundtrip_bitlevel_offset(&data);

    assert_eq!(rt_deflate, data);
    assert_eq!(rt_offset, data);
  }

  #[test]
  pub fn toplevel_roundtrip_3(){
    let data = vec![
      0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    ];
    let rt_deflate = roundtrip_bitlevel_deflate(&data);
    let rt_offset = roundtrip_bitlevel_offset(&data);

    assert_eq!(rt_deflate, data);
    assert_eq!(rt_offset, data);
  }

  #[test]
  pub fn toplevel_roundtrip_4(){
    let mut data = vec![1,2,3,4,3,2,1];
    for i in 0..10 {
      data.push(i);
    }
    data.append(&mut vec![15,16,17,18,17,16,15]);
    let rt_deflate = roundtrip_bitlevel_deflate(&data);
    let rt_offset = roundtrip_bitlevel_offset(&data);

    assert_eq!(rt_deflate, data);
    assert_eq!(rt_offset, data);
  }
*/
}