use super::default_data::default_codepoints::DecodeInfo;
use super::*;
use crate::huff_tree::huffcode_from_freqs;
use bitstream_io::{huffman::compile_write_tree, BitWriter, LittleEndian};
use lazy_static::lazy_static;
use std::io::Write;
use std::{
  collections::{HashMap, VecDeque},
  hash::Hash,
};
use thiserror::Error;

const MAX_HUFF_LEN: Option<usize> = Some(15);
const MAX_LZ_LEN: usize = 258;
lazy_static! {
  static ref DEFAULT_CODEPOINTS: DecodeInfo = DecodeInfo::new();
}

type BackrefMap<S> = HashMap<S, VecDeque<Index>>;

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
  const CHUNK_SZ: Len = 3;
  const MAX_MATCH_DIST: Dist = 16384;

  /// Generate a new CompressedBlock by performing LZ77 factorization on a given data block
  /// using the procedure presented in Section 4 of RFC 1951
  pub fn bytes_to_lz77(data: &Vec<u8>) -> Self {
    let mut match_table = HashMap::<&[u8], VecDeque<Index>>::new();
    let mut output: Vec<DeflateSym> = Vec::new();

    let mut index = 0usize;

    // Main compression loop. Follow sections 4's suggestion and start by looking at triplets, then
    // expanding matches as needed. This loop does not guarantee that all input has been encoded
    // when it ends, hence the cleanup section afterwards.
    while index + Self::CHUNK_SZ < data.len() {
      let (adv, mut matches) = Self::find_deflate_backref(&mut match_table, &data, index);
      index += adv;
      output.append(&mut matches);
    }

    // Cleanup: emit the last few unconsumed elements as literals (if needed).
    while index < data.len() {
      output.push(DeflateSym::Literal(data[index]));
      index += 1
    }
    Self { data: output }
  }

  /// Assuming a match of size length-1 succeeded at this index, does a match of length succeed?
  #[inline]
  fn check_match_valid(data: &[u8], data_start: Index, match_start: Index, length: Len) -> bool {
    assert!(data_start > match_start); // Assert that we're looking back, not forwards
    let no_data_overrun = data_start + length < data.len();
    let match_no_overlap = match_start + length <= data_start;
    let data_match = data[data_start + length - 1] == data[match_start + length - 1];
    no_data_overrun && match_no_overlap && data_match
  }

  fn longest_match_at_index(data: &[u8], data_start: Index, match_start: Index) -> Len {
    let mut len = 3;
    assert_eq!(
      data[data_start..data_start + len],
      data[match_start..match_start + len]
    );
    while len < MAX_LZ_LEN && Self::check_match_valid(data, data_start, match_start, len + 1) {
      len += 1;
    }
    len
  }

  fn find_longest_match(data: &[u8], start: Index, match_indices: &Vec<&Index>) -> (Dist, Len) {
    let mut max_len = 0usize;
    let mut max_index = 0usize;
    assert!(!match_indices.is_empty());
    for m in match_indices {
      let i = **m;
      let len = Self::longest_match_at_index(data, start, i);
      if len > max_len {
        max_len = len;
        max_index = i;
      }
    }
    assert_ne!(max_len, 0);
    assert!(start > max_index);
    assert!(max_len >= 3);
    (start - max_index, max_len)
  }

  /// Find a DEFLATE backref for this location in the input. These are the backrefs used
  /// by DEFLATE: <length, distance> pairs. Returns a tuple containing the DEFLATE symbol(s)
  /// to add to the output along with a usize indicating how far to advance the input stream.
  fn find_deflate_backref<'a>(
    match_table: &mut BackrefMap<&'a[u8]>,
    data: &'a [u8],
    index: usize,
  ) -> (usize, Vec<DeflateSym>) {
    let mut output = Vec::new();
    let mut n_consumed = 0usize;
    let term = &data[index..index + Self::CHUNK_SZ];

    Self::prune_matches(match_table, term, index, Self::MAX_MATCH_DIST);
    let matches = Self::get_matches(&match_table, term);

    // If no match, add this entry to the match table and emit a literal symbol
    if matches.is_empty() {
      match_table.entry(term).or_default().push_front(index);
      output.push(DeflateSym::Literal(data[index]));
      return (1, output);
    }

    // Find longest match starting at this index
    let (dist1, len1) = Self::find_longest_match(data, index, &matches);

    // Follow the deflate suggestion about deferred matching
    // This is technically wrong since we're looking back too far by 1, but
    // since our max match isn't the DEFLATE maximum, it's a non-issue atm.
    let search_term_2 = &data[index + 1..index + 1 + Self::CHUNK_SZ];
    let matches2 = Self::get_matches(&match_table, search_term_2);
    let (dist2, len2) = if !matches2.is_empty() {
      Self::find_longest_match(data, index + 1, &matches2)
    } else {
      (0, 0)
    };

    let (mdist, mlen, term, index) = if len2 > len1 {
      // Emit a literal and then the second match
      output.push(DeflateSym::Literal(data[index]));
      n_consumed += 1;
      (dist2, len2, search_term_2, index + 1)
    } else {
      (dist1, len1, term, index)
    };

    assert!(&data[index - mdist..index - mdist + mlen] == &data[index..index + mlen]);
    match_table.entry(term).or_default().push_front(index);

    n_consumed += mlen;
    output.push(DeflateSym::Backreference(mlen as u16, mdist as u16));

    (n_consumed, output)
  }

  /// Collect a vec from the VecDeque in the backref map
  fn get_matches<S: Hash + Eq>(backref_map: &BackrefMap<S>, term: S) -> Vec<&Index> {
    match backref_map.get(&term) {
      None => Vec::new(),
      Some(x) => x.iter().collect(),
    }
  }

  /// Prune all matches in the selected hashmap that are too old to participate in backrefs
  fn prune_matches<S: Hash + Eq>(
    backref_map: &mut BackrefMap<S>,
    term: S,
    cur_index: usize,
    max_dist: usize,
  ) {
    if let Some(x) = backref_map.get_mut(&term) {
      for i in (0..x.len()).rev() {
        if cur_index - x[i] > max_dist {
          x.pop_back().unwrap();
        } else {
          break; // Sorted order = rest are within range
        }
      }
    }
  }

  fn compute_lengthlit_freq(&self) -> HashMap<u16, usize> {
    let mut freqs = HashMap::new();
    for x in self.data.iter() {
      match x {
        DeflateSym::Literal(sym) => *freqs.entry(*sym as u16).or_default() += 1,
        DeflateSym::EndOfBlock => *freqs.entry(256).or_default() += 1,
        DeflateSym::Backreference(length, _) => {
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
        DeflateSym::Backreference(_, dist) => {
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
  pub fn new_from_raw_bytes(data: &Vec<u8>) -> Self {
    Self {
      blocks: vec![Block {
        bfinal: true,
        data: BlockData::Dyn(CompressedBlock::bytes_to_lz77(&data)),
      }],
    }
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
