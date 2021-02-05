use super::codepoints::DEFAULT_CODEPOINTS;
use super::default_data::default_huffcode::*;
use super::{Block, BlockData, CompressedBlock, DeflateStream, DeflateSym, DeflateWriteTree};
use crate::deflate::deflate_header::write_header;
use crate::deflate::lz77::encoder::do_lz77;

use bitstream_io::{huffman::compile_write_tree, BitWrite, BitWriter, HuffmanWrite, LittleEndian};
use thiserror::Error;

use std::collections::HashMap;
use std::io::Write;

#[derive(Error, Debug)]
pub enum DeflateWriteError {
  #[error("Length was out of range: {0}")]
  LengthOutOfRange(u16),
  #[error("Length was out of range: {0}")]
  DistOutOfRange(u16),
  #[error("Other IO Error: {0}")]
  IOError(#[from] std::io::Error),
}

impl DeflateSym {
  pub fn write_to_stream<W: Write>(
    &self,
    bit_sink: &mut BitWriter<W, LittleEndian>,
    length_tree: &DeflateWriteTree,
    dist_tree: &DeflateWriteTree,
  ) -> Result<(), DeflateWriteError> {
    match self {
      DeflateSym::Literal(x) => bit_sink.write_huffman(length_tree, (*x) as u16)?,
      DeflateSym::EndOfBlock => bit_sink.write_huffman(length_tree, 256)?,
      DeflateSym::Backreference(len, dist) => {
        DEFAULT_CODEPOINTS.write_length(*len, length_tree, bit_sink)?;
        DEFAULT_CODEPOINTS.write_dist(*dist, dist_tree, bit_sink)?;
      }
      DeflateSym::OffsetBackref(offset, len, dist) => {
        DEFAULT_CODEPOINTS.write_offset(*offset, length_tree, bit_sink)?;
        DEFAULT_CODEPOINTS.write_length(*len, length_tree, bit_sink)?;
        DEFAULT_CODEPOINTS.write_dist(*dist, dist_tree, bit_sink)?;
      }
    }
    Ok(())
  }
}

impl CompressedBlock {
  fn from_lz77_stream(syms: Vec<DeflateSym>) -> Self {
    let (lenlit_code, dist_code) = crate::deflate::lz77::encoder::compute_codes(&syms[..]);
    Self {
      lenlit_code,
      dist_code,
      data: syms,
    }
  }

  /// Generate a new CompressedBlock by performing LZ77 factorization on a given data block
  /// using the procedure presented in Section 4 of RFC 1951
  pub fn bytes_to_lz77(data: &[u8]) -> Self {
    let data = do_lz77(data, false);
    Self::from_lz77_stream(data)
  }

  /// Generate a new CompressedBlock by performing LZ77 factorization with the
  /// custom offset protocol described above.
  pub fn bytes_to_lz77_offset(data: &[u8]) -> Self {
    let data = do_lz77(data, true);
    Self::from_lz77_stream(data)
  }

  pub fn get_deflatesym_stream(&self) -> &[DeflateSym] {
    &self.data[..]
  }

  pub fn write_bitstream_fixed<W: Write>(
    &self,
    bit_sink: &mut BitWriter<W, LittleEndian>,
  ) -> Result<(), DeflateWriteError> {
    let length_codes = DEFAULT_LENGTH_CODE.clone();
    let dist_codes = DEFAULT_DIST_CODE.clone();
    let length_tree =
      compile_write_tree(length_codes).expect("Could not compile default length tree!");
    let dist_tree = compile_write_tree(dist_codes).expect("Could not compile default dist tree!");
    for sym in self.data.iter() {
      sym.write_to_stream(bit_sink, &length_tree, &dist_tree)?;
    }
    Ok(())
  }

  pub fn write_bitstream_dynamic<W: Write>(
    &self,
    bit_sink: &mut BitWriter<W, LittleEndian>,
  ) -> Result<(), DeflateWriteError> {
    write_header(bit_sink, &self.lenlit_code, &self.dist_code)?;

    let length_tree = compile_write_tree(self.lenlit_code.clone()).unwrap();
    let dist_tree = compile_write_tree(self.dist_code.clone()).unwrap();

    for sym in self.data.iter() {
      sym.write_to_stream(bit_sink, &length_tree, &dist_tree)?;
    }
    Ok(())
  }
}

impl BlockData {
  pub fn write_to_bitstream<W: Write>(
    &self,
    bit_sink: &mut BitWriter<W, LittleEndian>,
  ) -> Result<(), DeflateWriteError> {
    match self {
      Self::Raw(_x) => {
        bit_sink.write(2, 0b00)?;
        unimplemented!();
      }
      Self::Fix(x) => {
        bit_sink.write(2, 0b01)?;
        x.write_bitstream_fixed(bit_sink)
      }
      Self::Dyn(x) => {
        bit_sink.write(2, 0b10)?;
        x.write_bitstream_dynamic(bit_sink)
      }
    }
  }
}

impl Block {
  pub fn write_to_bitstream<W: Write>(
    &self,
    bit_sink: &mut BitWriter<W, LittleEndian>,
  ) -> Result<(), DeflateWriteError> {
    bit_sink.write_bit(self.bfinal)?;
    self.data.write_to_bitstream(bit_sink)
  }
}

impl DeflateStream {
  pub fn write_to_bitstream<W: Write>(&self, sink: W) -> Result<W, DeflateWriteError> {
    let mut bit_sink = BitWriter::new(sink);
    for b in self.blocks.iter() {
      b.write_to_bitstream(&mut bit_sink)?
    }
    // We must byte-align the writer or risk losing incomplete bytes
    bit_sink.byte_align()?;
    Ok(bit_sink.into_writer())
  }

  pub fn new_from_raw_bytes_deflate(data: &[u8]) -> Self {
    Self {
      blocks: vec![Block {
        bfinal: true,
        data: BlockData::Dyn(CompressedBlock::bytes_to_lz77(&data)),
      }],
    }
  }

  pub fn new_from_raw_bytes_offset(data: &[u8]) -> Self {
    Self {
      blocks: vec![Block {
        bfinal: true,
        data: BlockData::Dyn(CompressedBlock::bytes_to_lz77_offset(&data)),
      }],
    }
  }

  /// Gets a stream of DeflateSyms encoded by this block
  pub fn get_deflatesym_stream(&self) -> Vec<DeflateSym> {
    let mut syms = Vec::new();
    for block in self.blocks.iter() {
      let bsyms = match &block.data {
        BlockData::Raw(u) => u.data.iter().cloned().map(DeflateSym::Literal).collect(),
        BlockData::Fix(b) | BlockData::Dyn(b) => b.data.clone(),
      };
      syms.extend(bsyms);
    }
    syms
  }
}

#[cfg(test)]
mod tests {
  #[allow(unused_imports)]
  use super::*;
  use rand::Rng;

  #[test]
  fn round_trip_deflate_1() {
    let init_str = "hellohellohelloIamGeronimohello";
    let data = init_str.to_owned().into_bytes();
    let comp = CompressedBlock::bytes_to_lz77(&data);
    let mut rt = Vec::new();
    comp.into_decompressed_bytes(&mut rt).unwrap();
    let fini_str = std::str::from_utf8(&rt).unwrap();
    assert_eq!(init_str, fini_str);
  }

  #[test]
  fn round_trip_deflate_2() {
    let init_str = "Entire any had depend and figure winter. Change stairs and men likely wisdom new happen piqued six. Now taken him timed sex world get. Enjoyed married an feeling delight pursuit as offered. As admire roused length likely played pretty to no. Means had joy miles her merry solid order.";
    let data = init_str.to_owned().into_bytes();
    let comp = CompressedBlock::bytes_to_lz77(&data);
    let mut rt = Vec::new();
    comp.into_decompressed_bytes(&mut rt).unwrap();
    let fini_str = std::str::from_utf8(&rt).unwrap();
    assert_eq!(init_str, fini_str);
  }

  #[test]
  fn round_trip_offset_1() {
    let init_str = "hellohellohelloIamGeronimohello";
    let data = init_str.to_owned().into_bytes();
    let comp = CompressedBlock::bytes_to_lz77_offset(&data);
    let mut rt = Vec::new();
    comp.into_decompressed_bytes(&mut rt).unwrap();
    let fini_str = std::str::from_utf8(&rt).unwrap();
    assert_eq!(init_str, fini_str);
  }

  #[test]
  fn round_trip_offset_2() {
    let init_str = "Entire any had depend and figure winter. Change stairs and men likely wisdom new happen piqued six. Now taken him timed sex world get. Enjoyed married an feeling delight pursuit as offered. As admire roused length likely played pretty to no. Means had joy miles her merry solid order.";
    let data = init_str.to_owned().into_bytes();
    let comp = CompressedBlock::bytes_to_lz77_offset(&data);
    let mut rt = Vec::new();
    comp.into_decompressed_bytes(&mut rt).unwrap();
    let fini_str = std::str::from_utf8(&rt).unwrap();
    assert_eq!(init_str, fini_str);
  }

  #[test]
  fn offset_should_create_offsetbr_1() {
    let data = vec![
      0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    ];
    let comp = CompressedBlock::bytes_to_lz77_offset(&data);
    let symstream = comp.data;
    let answer = [
      DeflateSym::Literal(0),
      DeflateSym::Literal(1),
      DeflateSym::Literal(2),
      DeflateSym::Literal(3),
      DeflateSym::Literal(4),
      DeflateSym::Literal(5),
      DeflateSym::Literal(6),
      DeflateSym::Literal(7),
      DeflateSym::Literal(8),
      DeflateSym::OffsetBackref(9, 8, 8),
      DeflateSym::Literal(18),
      DeflateSym::EndOfBlock,
    ];
    assert_eq!(symstream, answer);
  }

  #[test]
  fn simple_offsetbr_roundtrip() {
    let data = vec![
      0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    ];
    let comp = CompressedBlock::bytes_to_lz77_offset(&data);
    let mut rt = Vec::new();
    comp.into_decompressed_bytes(&mut rt).unwrap();
    assert_eq!(data, rt);
  }

  #[test]
  fn round_trip_offset() {
    let mut testvec = vec![1, 2, 3, 4, 3, 2, 1];
    for i in 0..10 {
      testvec.push(i);
    }
    testvec.append(&mut vec![15, 16, 17, 18, 17, 16, 15]);
    let comp = CompressedBlock::bytes_to_lz77_offset(&testvec);
    println!("{:?}", comp.data);
    assert!(comp.data.iter().any(|x| match x {
      DeflateSym::OffsetBackref(_, _, _) => true,
      _ => false,
    }));
    let mut rt = Vec::new();
    comp.into_decompressed_bytes(&mut rt).unwrap();

    if rt != testvec {
      println!(
        "Compressed block: {:?}",
        CompressedBlock::bytes_to_lz77_offset(&testvec)
      );
      println!("Result: {:?}", rt);
      assert_eq!(rt, testvec);
    }
    let mut rng = rand::thread_rng();
    let mut testvec = Vec::new();
    for _ in 0..1024 {
      testvec.push(rng.gen::<u8>());
    }
  }

  #[test]
  fn round_trip_randomlike_deflate() {
    let mut rng = rand::thread_rng();
    let mut testvec = vec![1, 2, 3, 4, 3, 2, 1];
    for _ in 0..100 {
      testvec.push(rng.gen::<u8>());
    }
    testvec.append(&mut vec![15, 16, 17, 18, 17, 16, 15]);
    let comp = CompressedBlock::bytes_to_lz77(&testvec);

    let mut rt = Vec::new();
    comp.into_decompressed_bytes(&mut rt).unwrap();

    if rt != testvec {
      println!(
        "Compressed block: {:?}",
        CompressedBlock::bytes_to_lz77(&testvec)
      );
      println!("Result: {:?}", rt);
      assert_eq!(rt, testvec);
    }
  }

  #[test]
  /// Make sure that, even past a huge glob of claptrap, we're still triggering
  /// the offset detector.
  fn round_trip_randomlike_offset() {
    let mut rng = rand::thread_rng();
    let mut testvec = vec![1, 2, 3, 4, 3, 2, 1];
    for _ in 0..100 {
      testvec.push(rng.gen::<u8>());
    }
    testvec.append(&mut vec![15, 16, 17, 18, 17, 16, 15]);
    let comp = CompressedBlock::bytes_to_lz77_offset(&testvec);

    assert!(comp.data.iter().any(|x| match x {
      DeflateSym::OffsetBackref(_, _, _) => true,
      _ => false,
    }));
    let mut rt = Vec::new();
    comp.into_decompressed_bytes(&mut rt).unwrap();

    if rt != testvec {
      println!(
        "Compressed block: {:?}",
        CompressedBlock::bytes_to_lz77(&testvec)
      );
      println!("Result: {:?}", rt);
      assert_eq!(rt, testvec);
    }
  }

  #[test]
  fn many_random_round_trips_deflate() {
    for _ in 0..10 {
      round_trip_randomlike_deflate()
    }
  }
}
