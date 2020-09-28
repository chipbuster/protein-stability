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

/* Custom DEFLATE extension: an offset backref. This is similar to a DEFLATE
backref, but the entire sequence is offset by a fixed amount, modulo 255.

That is, to compute the expansion of an offset backref <off, length, dist>, do
the same thing with the <length, dist> values as you would for an ordinary DEFLATE
backreference, then add offset (mod 255) to every element of the new sequence.

The presence of an offset backref is indicated in the compressed sequence by
the symbol value 259 in the length/literal encoding. This is followed by the
offset value encoded directly in the length/literal encoding, then the
length and distance as is found in the standard DEFLATE encoding.
*/

const MAX_HUFF_LEN: Option<usize> = Some(15);
const MAX_LZ_LEN: usize = 258;
const OFFSET_SIGIL: u16 = 259;
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
      DeflateSym::OffsetBackref(offset, length, dist) => {
        bit_sink.write_huffman(length_tree, OFFSET_SIGIL)?;
        bit_sink.write_huffman(length_tree, *offset as u16)?;
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
    Self::do_lz77(data, false)
  }

  pub fn bytes_to_lz77_offset(data: &Vec<u8>) -> Self {
    Self::do_lz77(data, true)
  }

  fn do_lz77(data: &Vec<u8>, use_offset: bool) -> Self {
    let mut deflate_match_table = HashMap::<&[u8], VecDeque<Index>>::new();
    let mut offset_match_table = HashMap::<(u8, u8), VecDeque<Index>>::new();
    let mut output: Vec<DeflateSym> = Vec::new();

    let mut index = 0usize;

    // Main compression loop. Follow sections 4's suggestion and start by looking at triplets, then
    // expanding matches as needed. This loop does not guarantee that all input has been encoded
    // when it ends, hence the cleanup section afterwards.
    while index + Self::CHUNK_SZ < data.len() {
      let (deflate_len, mut deflate_syms) = Self::find_deflate_backref(&mut deflate_match_table, &data, index);
      let mut offset_match = if use_offset {
        Self::find_offset_backref(&mut offset_match_table, &data, index)
      } else {
        None
      };

      // Select the match to use based on input options and match lengths
      let (match_len, mut match_syms) = if let Some((off_len, mut off_syms)) = offset_match {
        if off_len > deflate_len + 5 {
          (off_len, off_syms)
        } else {
          (deflate_len, deflate_syms)
        }
      } else {
        (deflate_len, deflate_syms)
      };
      
      index += match_len;
      output.append(&mut match_syms);
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
  fn check_match_valid(
    data: &[u8],
    data_start: Index,
    match_start: Index,
    length: Len,
    offset: u8,
  ) -> bool {
    assert!(data_start > match_start); // Assert that we're looking back, not forwards
    let no_data_overrun = data_start + length < data.len();
    let match_no_overlap = match_start + length <= data_start;
    let data_match =
      data[data_start + length - 1] == data[match_start + length - 1].wrapping_add(offset);
    no_data_overrun && match_no_overlap && data_match
  }

  fn longest_match_at_index(data: &[u8], data_start: Index, match_start: Index, offset: u8) -> Len {
    let mut len = 3;
    assert_eq!(len, 3); // If the min match length changes, the offset code will need to change
    if offset == 0 {
      assert_eq!(
        data[data_start..data_start + len],
        data[match_start..match_start + len]
      );
    } else {
      let data_init = (data[data_start], data[data_start + 1], data[data_start + 2]);
      let match_init = (
        data[match_start].wrapping_add(offset),
        data[match_start + 1].wrapping_add(offset),
        data[match_start + 2].wrapping_add(offset),
      );
      assert_eq!(data_init, match_init);
    }
    while len < MAX_LZ_LEN
      && Self::check_match_valid(data, data_start, match_start, len + 1, offset)
    {
      len += 1;
    }
    len
  }

  fn find_longest_match(
    data: &[u8],
    start: Index,
    match_indices: &Vec<&Index>,
    use_offset: bool,
  ) -> (Dist, Len) {
    let mut max_len = 0usize;
    let mut max_index = 0usize;
    assert!(!match_indices.is_empty());
    for m in match_indices {
      let i = **m;
      let len = if use_offset {
        unimplemented!();
        let offset = data[i].wrapping_sub(data[start]);
        Self::longest_match_at_index(data, start, i, offset)
      } else {
        Self::longest_match_at_index(data, start, i, 0)
      };
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

  /// Find an offset backref for this location in the input. These are custom backrefs:
  /// <offset, length, distance> pairs. Returns a tuple containing the symbol(s) to add
  /// to the output along with a usize indicating how far to advance the input stream.
  // This function is similar to find_deflate_backref, but the differences are fairly crucial
  fn find_offset_backref<'a>(
    match_table: &mut BackrefMap<(u8, u8)>,
    data: &'a [u8],
    index: usize,
  ) -> Option<(usize, Vec<DeflateSym>)> {
    // Note: if CHUNK_SZ ever changes from 3, we need to change the type of match_table
    assert_eq!(Self::CHUNK_SZ, 3);

    let base = data[index];
    let term = (data[index + 1] - base, data[index + 2] - base);

    let mut output = Vec::new();
    let mut n_consumed = 0usize;

    Self::prune_matches(match_table, term, index, Self::MAX_MATCH_DIST);
    let matches = Self::get_matches(match_table, term);

    // We don't *have* to find a match for offset backrefs (because they're an optional feature).
    // Therefore, we return None to signal that no match could be found and let the DEFLATE backref
    // function deal with the emitting of literal single bytes.
    if matches.is_empty() {
      return None;
    };

    let (dist1, len1) = Self::find_longest_match(data, index, &matches, true);

    // Search at index = index + 1 for a better match
    let term2 = (data[index + 2] - base, data[index + 3] - base);
    let matches2 = Self::get_matches(&match_table, term2);
    let (dist2, len2) = if !matches2.is_empty() {
      Self::find_longest_match(data, index + 1, &matches2, true)
    } else {
      (0, 0)
    };

    let (mdist, mlen, term, index) = if len2 > len1 {
      // Emit a literal and then the second match
      output.push(DeflateSym::Literal(data[index]));
      n_consumed += 1;
      (dist2, len2, term2, index + 1)
    } else {
      (dist1, len1, term, index)
    };

    match_table.entry(term).or_default().push_front(index);

    n_consumed += mlen;
    output.push(DeflateSym::Backreference(mlen as u16, mdist as u16));

    // Debugging output here
    let offset = data[index].wrapping_sub(data[index - mlen]);
    println!("Found offset match at ({}, {}, {})", offset, mlen, mdist);
    println!("Current data is {:?}", &data[index..index + mlen]);
    println!(
      "Current data is {:?}",
      &data[index - mdist..index - mdist + mlen]
    );

    Some((n_consumed, output))
  }

  /// Find a DEFLATE backref for this location in the input. These are the backrefs used
  /// by DEFLATE: <length, distance> pairs. Returns a tuple containing the DEFLATE symbol(s)
  /// to add to the output along with a usize indicating how far to advance the input stream.
  fn find_deflate_backref<'a>(
    match_table: &mut BackrefMap<&'a [u8]>,
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
    let (dist1, len1) = Self::find_longest_match(data, index, &matches, false);

    // Follow the deflate suggestion about deferred matching
    // This is technically wrong since we're looking back too far by 1, but
    // since our max match isn't the DEFLATE maximum, it's a non-issue atm.
    let search_term_2 = &data[index + 1..index + 1 + Self::CHUNK_SZ];
    let matches2 = Self::get_matches(&match_table, search_term_2);
    let (dist2, len2) = if !matches2.is_empty() {
      Self::find_longest_match(data, index + 1, &matches2, false)
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
        DeflateSym::OffsetBackref(offset, length, _) => {
          let lc = &DEFAULT_CODEPOINTS.lookup_length(*length).unwrap().codept;
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
        DeflateSym::Backreference(_, dist) | DeflateSym::OffsetBackref(_, _, dist) => {
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
