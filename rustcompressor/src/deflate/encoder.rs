use super::default_data::default_codepoints::DecodeInfo;
use super::*;
use bitstream_io::{BitWriter, LittleEndian};
use std::collections::{HashMap, VecDeque};
use std::{hash::Hash, io::Write};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum DeflateWriteError {
  #[error("Other IO Error: {0}")]
  IOError(#[from] std::io::Error),
}

fn write_uncompressed_block(
  data: &[DeflateSym],
  lit_tree: &DeflateWriteTree,
  dist_tree: &DeflateWriteTree,
) -> Result<(), DeflateWriteError> {
  // Seriously, don't implement this until you have to
  unimplemented!()
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
      let matches: Vec<&Index> = match match_table.get(search_term) {
        None => Vec::new(),
        Some(x) => x.iter().filter(|x| index - **x < MAX_MATCH_DIST).collect(),
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
        (0,0)
      };

      let (mdist, mlen) = if len2 > len1 {
        // Emit a literal and then the second match
        output.push(DeflateSym::Literal(data[index]));
        index += 1;
        (dist2, len2)
      } else {
        (dist1, len1)
      };

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
}

impl DeflateStream {
  pub fn write_bitstream_with_trees<W: Write>(
    &self,
    bit_sink: W,
    lit_tree: &DeflateWriteTree,
    dist_tree: &DeflateWriteTree,
  ) -> Result<(), DeflateWriteError> {
    unimplemented!()
  }
  pub fn write_bitstream_with_custom<W: Write>(
    &self,
    bit_sink: W,
  ) -> Result<(), DeflateWriteError> {
    unimplemented!()
  }
  pub fn write_bitstream_with_default<W: Write>(
    &self,
    bit_sink: W,
  ) -> Result<(), DeflateWriteError> {
    unimplemented!()
  }
  pub fn gen_default_trees(&self) {}
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
      println!("Compressed block: {:?}", CompressedBlock::bytes_to_lz77(&testvec));
      println!("Result: {:?}", rt);
      assert_eq!(rt, testvec);
    }
  }

  #[test]
  fn many_random_round_trips() {
    for _ in 0..100{
      round_trip_randomlike()
    }
  }
}
