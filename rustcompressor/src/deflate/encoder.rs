use super::default_data::default_codepoints::{DecodeCodepoint, DecodeInfo};
use super::*;
use crate::huff_tree::huffcode_from_freqs;
use bitstream_io::{huffman::compile_write_tree, BitWriter, LittleEndian};
use lazy_static::lazy_static;
use std::collections::{HashMap, VecDeque};
use std::{hash::Hash, io::Write};
use thiserror::Error;

const MAX_HUFF_LEN: Option<usize> = Some(15);
lazy_static! {
  static ref DEFAULT_CODEPOINTS: DecodeInfo = DecodeInfo::new();
}

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
    dist_tree: Option<&DeflateWriteTree>,
  ) -> Result<(), DeflateWriteError> {
    match self {
      DeflateSym::EndOfBlock => bit_sink.write_huffman(length_tree, 256)?,
      DeflateSym::Literal(sym) => bit_sink.write_huffman(length_tree, *sym as u16)?,
      DeflateSym::Backreference(length, dist) => {
        Self::write_length_to_stream(bit_sink, *length, length_tree)?;
        Self::write_dist_to_stream(bit_sink, *dist, dist_tree)?;
      }
    }
    Ok(())
  }

  fn write_dist_to_stream<W: Write>(
    bit_sink: &mut BitWriter<W, LittleEndian>,
    dist: u16,
    dist_tree: Option<&DeflateWriteTree>,
  ) -> Result<(), DeflateWriteError> {
    if let Some(c) = &DEFAULT_CODEPOINTS.lookup_dist(dist) {
      let extra_bits = dist - c.lo;
      if let Some(d) = dist_tree {
        bit_sink.write_huffman(d, c.codept)?;
      } else {
        let temp = c.codept;
        let mut out = 0u16;
        out |= (temp & 0b00001) << 4;
        out |= (temp & 0b00010) << 2;
        out |= temp & 0b00100;
        out |= (temp & 0b01000) >> 2;
        out |= (temp & 0b10000) >> 4;
        bit_sink.write(5, out)?;
      }
      bit_sink.write(c.nbits as u32, extra_bits)?;
      return Ok(());
    }
    Err(DeflateWriteError::DistOutOfRange(dist))
  }

  fn write_length_to_stream<W: Write>(
    bit_sink: &mut BitWriter<W, LittleEndian>,
    length: u16,
    length_tree: &DeflateWriteTree,
  ) -> Result<(), DeflateWriteError> {
    if let Some(c) = &DEFAULT_CODEPOINTS.lookup_length(length) {
      let extra_bits = length - c.lo;
      bit_sink.write_huffman(length_tree, c.codept)?;
      bit_sink.write(c.nbits as u32, extra_bits)?;
      return Ok(());
    }
    Err(DeflateWriteError::LengthOutOfRange(length))
  }
}

type Dist = usize;
type Len = usize;
type Index = usize;

impl CompressedBlock {
  /// Generate a new CompressedBlock by performing LZ77 factorization on a given data block
  /// using the procedure presented in Section 4 of RFC 1951
  pub fn bytes_to_lz77(data: &Vec<u8>) -> Self {
    const CHUNK_SZ: Len = 3;
    const MAX_MATCH_DIST: Dist = 16384;

    let data_slice = &data[..];
    let mut match_table = HashMap::<&[u8], VecDeque<Index>>::new();
    let mut output: Vec<DeflateSym> = Vec::new();

    let mut index = 0usize;
    let mut cur_match_length = 0usize;
    let mut cur_match_dist = 0usize;

    // Main compression loop. Follow sections 4's suggestion and start by looking at triplets, then
    // expanding matches as needed. This loop does not guarantee that all input has been encoded
    // when it ends, hence the cleanup section afterwards.
    while index + CHUNK_SZ < data.len() {
      let search_term = &data[index..index + CHUNK_SZ];
      let matches: Vec<&Index> = match match_table.get_mut(search_term) {
        None => Vec::new(),
        Some(x) => {
          // Filter out the elements that are too far away
          for i in (0..x.len()).rev() {
            if index - x[i] > MAX_MATCH_DIST {
              x.pop_back().unwrap();
            } else {
              break; // Sorted order = rest are within range
            }
          }
          x.iter().collect()
        }
      };

      if matches.is_empty() {
        // Add this entry to the match table
        match_table
          .entry(search_term)
          .or_default()
          .push_front(index);
        // Emit a literal symbol
        output.push(DeflateSym::Literal(data[index]));
        index += 1;
        continue;
      }

      // Find longest match starting at this index
      let (dist1, len1) = Self::find_longest_match(data_slice, index, &matches);

      // Find longest match starting at next index
      let search_term_2 = &data[index + 1..index + 1 + CHUNK_SZ];
      let matches2 = match match_table.get(search_term_2) {
        None => Vec::new(),
        Some(x) => x.iter().filter(|x| index - **x < MAX_MATCH_DIST).collect(),
      };
      let (dist2, len2) = if !matches2.is_empty() {
        Self::find_longest_match(data_slice, index + 1, &matches2)
      } else {
        (0, 0)
      };

      let (mdist, mlen) = if len2 > len1 {
        // Emit a literal and then the second match
        output.push(DeflateSym::Literal(data[index]));
        index += 1;
        (dist2, len2)
      } else {
        (dist1, len1)
      };

      // Add this match to the match table
      match_table
        .entry(search_term)
        .or_default()
        .push_front(index);

      index += mlen;
      output.push(DeflateSym::Backreference(mlen as u16, mdist as u16));
    }

    // Cleanup: emit the last few unconsumed elements as literals (if needed).
    while index < data.len() {
      output.push(DeflateSym::Literal(data[index]));
      index += 1
    }
    Self { data: output }
  }

  fn find_longest_match(data: &[u8], start: Index, match_indices: &Vec<&Index>) -> (Dist, Len) {
    let mut max_len = 0usize;
    let mut max_index = 0usize;
    assert!(!match_indices.is_empty());
    for m in match_indices {
      let i = **m;
      let mut len = 3;
      assert_eq!(data[start..start + len], data[i..i + len]);
      let mut range_valid = start + len < data.len() && i + len < start;
      let mut ranges_match = data[start..start + len] == data[i..i + len];
      while range_valid && ranges_match {
        len += 1;
        range_valid = start + len < data.len() && i + len < start;
        ranges_match = data[start..start + len] == data[i..i + len];
      }
      if len > max_len {
        max_len = len;
        max_index = i;
      }
    }
    assert_ne!(max_len, 0);
    assert!(start > max_index);
    (start - max_index, max_len - 1)
  }

  fn compute_lengthlit_freq(&self) -> HashMap<u16, usize> {
    let mut freqs = HashMap::new();
    for x in self.data.iter() {
      match x {
        DeflateSym::Literal(sym) => *freqs.entry(*sym as u16).or_default() += 1,
        DeflateSym::EndOfBlock => *freqs.entry(256).or_default() += 1,
        DeflateSym::Backreference(length, dist) => {
          let lc = &DEFAULT_CODEPOINTS.lookup_length(*length).unwrap().codept;
          *freqs.entry(*lc).or_default() += 1;
        }
      }
    }
    freqs
  }

  fn compute_dist_freq(&self) -> HashMap<u16, usize> {
    let mut freqs = HashMap::new();
    for x in self.data.iter() {
      match x {
        DeflateSym::Literal(_) | DeflateSym::EndOfBlock => continue,
        DeflateSym::Backreference(length, dist) => {
          let dc = &DEFAULT_CODEPOINTS.lookup_dist(*dist).unwrap().codept;
          *freqs.entry(*dc).or_default() += 1;
        }
      }
    }
    freqs
  }

  pub fn write_bitstream_fixed<W: Write>(
    &self,
    bit_sink: &mut BitWriter<W, LittleEndian>,
  ) -> Result<(), DeflateWriteError> {
    let length_tree = super::default_data::default_hufftree::default_write_hufftree();
    for sym in self.data.iter() {
      sym.write_to_stream(bit_sink, &length_tree, None)?;
    }
    Ok(())
  }

  pub fn write_bitstream_dynamic<W: Write>(
    &self,
    bit_sink: &mut BitWriter<W, LittleEndian>,
  ) -> Result<(), DeflateWriteError> {
    let lit_freq = self.compute_lengthlit_freq();
    let dist_freq = self.compute_dist_freq();
    let length_codes = huffcode_from_freqs(&lit_freq, MAX_HUFF_LEN);
    let dist_codes = huffcode_from_freqs(&dist_freq, MAX_HUFF_LEN);
    let length_tree = compile_write_tree(length_codes).unwrap();
    let dist_tree = compile_write_tree(dist_codes).unwrap();

    for sym in self.data.iter() {
      sym.write_to_stream(bit_sink, &length_tree, Some(&dist_tree))?;
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
      Self::Raw(x) => {
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
}

#[cfg(test)]
mod tests {
  #[allow(unused_imports)]
  use super::*;
  use rand::Rng;

  #[test]
  fn round_trip() {
    let init_str = "hellohellohelloIamGeronimohello";
    let data = init_str.to_owned().into_bytes();
    let comp = CompressedBlock::bytes_to_lz77(&data);
    let rt = comp.into_bytes().unwrap();
    let fini_str = std::str::from_utf8(&rt).unwrap();
    assert_eq!(init_str, fini_str);
  }

  #[test]
  fn round_trip_2() {
    let init_str = "Entire any had depend and figure winter. Change stairs and men likely wisdom new happen piqued six. Now taken him timed sex world get. Enjoyed married an feeling delight pursuit as offered. As admire roused length likely played pretty to no. Means had joy miles her merry solid order.";
    let data = init_str.to_owned().into_bytes();
    let comp = CompressedBlock::bytes_to_lz77(&data);
    let rt = comp.into_bytes().unwrap();
    let fini_str = std::str::from_utf8(&rt).unwrap();
    assert_eq!(init_str, fini_str);
  }

  #[test]
  fn round_trip_randomlike() {
    let mut rng = rand::thread_rng();
    let mut testvec = Vec::new();
    for _ in 0..8192 {
      testvec.push(rng.gen::<u8>());
    }
    let comp = CompressedBlock::bytes_to_lz77(&testvec);
    let rt = comp.into_bytes().unwrap();

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
  fn many_random_round_trips() {
    for _ in 0..100 {
      round_trip_randomlike()
    }
  }
}
