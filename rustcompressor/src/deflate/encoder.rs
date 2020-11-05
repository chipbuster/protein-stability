use super::codepoints::{DEFAULT_CODEPOINTS, OFFSET_SIGIL};
use super::*;
use crate::huff_tree::huffcode_from_freqs;
use crate::deflate::deflate_header::write_header;
use bitstream_io::{huffman::compile_write_tree, BitWriter, LittleEndian};
use std::io::Write;
use std::{
  collections::{HashMap, VecDeque},
  hash::Hash,
};
use thiserror::Error;

const MAX_HUFF_LEN: Option<usize> = Some(15);
const MAX_LZ_LEN: usize = 258;

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
    println!("Writing {:?} to stream", self);
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

  /// Generate a new CompressedBlock by performing LZ77 factorization with the
  /// custom offset protocol described above.
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
      let (deflate_len, deflate_syms) =
        Self::find_deflate_backref(&mut deflate_match_table, &data, index);
      let offset_match = if use_offset {
        Self::find_offset_backref(&mut offset_match_table, &data, index)
      } else {
        None
      };

      /*
      println!("Data is {:?}", &data[index..index+3]);
      if let Some(x) = offset_match.as_ref(){
        println!("Match is {:?}", x.1);
      }
      println!("{}: {:?}, ({}, {:?})", index, offset_match, deflate_len, deflate_syms);
      */

      // Select the match to use based on input options and match lengths
      let (match_len, mut match_syms) = if let Some((off_len, off_syms)) = offset_match {
        if off_len > deflate_len + 3 {
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

    // Add an end-of-stream indicator
    output.push(DeflateSym::EndOfBlock);
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
    if !no_data_overrun || !match_no_overlap{
      return false;
    }
    let data_match =
      data[data_start + length - 1] == data[match_start + length - 1].wrapping_add(offset);
    data_match
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
        let offset = data[start].wrapping_sub(data[i]);
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
    let term = (
      data[index + 1].wrapping_sub(base),
      data[index + 2].wrapping_sub(base),
    );

    let mut output = Vec::new();
    let mut n_consumed = 0usize;

    Self::prune_matches(match_table, term, index, Self::MAX_MATCH_DIST);
    let matches = Self::get_matches(match_table, term, index);

    // We don't *have* to find a match for offset backrefs (because they're an optional feature).
    // Therefore, we return None to signal that no match could be found and let the DEFLATE backref
    // function deal with the emitting of literal single bytes.
    if matches.is_empty() {
      match_table.entry(term).or_default().push_front(index);
      return None;
    };

    let (dist1, len1) = Self::find_longest_match(data, index, &matches, true);

    // Search at index = index + 1 for a better match
    let term2 = (
      data[index + 2].wrapping_sub(data[index + 1]),
      data[index + 3].wrapping_sub(data[index + 1]),
    );

    Self::prune_matches(match_table, term2, index + 1, Self::MAX_MATCH_DIST);
    let matches2 = Self::get_matches(&match_table, term2, index);
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

    n_consumed += mlen;
    match_table.entry(term).or_default().push_front(index);

    /* We have intentionally avoided computing the offset in this funciton so far
    (leaving it for the subroutines to do), but now we need to know what it is
    so that we can add it to the output stream */
    let offset = data[index].wrapping_sub(data[index - mdist]);
    output.push(DeflateSym::OffsetBackref(offset, mlen as u16, mdist as u16));

    assert_eq!(data[index], data[index - mdist].wrapping_add(offset));
    assert!(mdist <= Self::MAX_MATCH_DIST);

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
    let matches = Self::get_matches(&match_table, term, index);

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
    Self::prune_matches(match_table, search_term_2, index + 1, Self::MAX_MATCH_DIST);
    let matches2 = Self::get_matches(&match_table, search_term_2, index);
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
    assert!(mdist <= Self::MAX_MATCH_DIST);
    match_table.entry(term).or_default().push_front(index);

    n_consumed += mlen;
    output.push(DeflateSym::Backreference(mlen as u16, mdist as u16));

    (n_consumed, output)
  }

  /// Collect a vec from the VecDeque in the backref map
  fn get_matches<S: Hash + Eq>(backref_map: &BackrefMap<S>, term: S, index: usize) -> Vec<&Index> {
    match backref_map.get(&term) {
      None => Vec::new(),
      Some(x) => x.iter().filter(|x| index - **x > Self::CHUNK_SZ).collect(),
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
          let lc = &DEFAULT_CODEPOINTS.get_codepoint_for_length(*length).code();
          *freqs.entry(*lc).or_default() += 1;
        }
        DeflateSym::OffsetBackref(offset, length, _) => {
          let lc = &DEFAULT_CODEPOINTS.get_codepoint_for_length(*length).code();
          *freqs.entry(*lc).or_default() += 1;
          *freqs.entry(*offset as u16).or_default() += 1;
          *freqs.entry(OFFSET_SIGIL).or_default() += 1;
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
          let dc = &DEFAULT_CODEPOINTS.get_codepoint_for_dist(*dist).code();
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
    let length_codes = huffcode_from_freqs(&lit_freq, MAX_HUFF_LEN);
    let length_tree = compile_write_tree(length_codes.clone()).unwrap();

    let dist_freq = self.compute_dist_freq();
    let dist_codes = huffcode_from_freqs(&dist_freq, MAX_HUFF_LEN);
    let dist_tree = if dist_freq.is_empty() {
        None
    } else {
        Some(compile_write_tree(dist_codes.clone()).expect("Could not compile write tree"))
    };

    write_header(bit_sink, &length_codes, &dist_codes)?;

    println!("Header Written!");

    for sym in self.data.iter() {
      sym.write_to_stream(bit_sink, &length_tree, dist_tree.as_ref())?;
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

  pub fn new_from_raw_bytes_deflate(data: &Vec<u8>) -> Self {
    Self {
      blocks: vec![Block {
        bfinal: true,
        data: BlockData::Dyn(CompressedBlock::bytes_to_lz77(&data)),
      }],
    }
  }

  pub fn new_from_raw_bytes_offset(data: &Vec<u8>) -> Self {
    Self {
      blocks: vec![Block {
        bfinal: true,
        data: BlockData::Dyn(CompressedBlock::bytes_to_lz77_offset(&data)),
      }],
    }
  }
}

#[cfg(test)]
mod tests {
  use std::iter::FromIterator;

  #[allow(unused_imports)]
  use super::*;
  use rand::Rng;


  #[test]
  fn round_trip_deflate_1() {
    let init_str = "hellohellohelloIamGeronimohello";
    let data = init_str.to_owned().into_bytes();
    let comp = CompressedBlock::bytes_to_lz77(&data);
    let rt = comp.into_decompressed_bytes().unwrap();
    let fini_str = std::str::from_utf8(&rt).unwrap();
    assert_eq!(init_str, fini_str);
  }

  #[test]
  fn round_trip_deflate_2() {
    let init_str = "Entire any had depend and figure winter. Change stairs and men likely wisdom new happen piqued six. Now taken him timed sex world get. Enjoyed married an feeling delight pursuit as offered. As admire roused length likely played pretty to no. Means had joy miles her merry solid order.";
    let data = init_str.to_owned().into_bytes();
    let comp = CompressedBlock::bytes_to_lz77(&data);
    let rt = comp.into_decompressed_bytes().unwrap();
    let fini_str = std::str::from_utf8(&rt).unwrap();
    assert_eq!(init_str, fini_str);
  }

  #[test]
  fn round_trip_offset_1() {
    let init_str = "hellohellohelloIamGeronimohello";
    let data = init_str.to_owned().into_bytes();
    let comp = CompressedBlock::bytes_to_lz77_offset(&data);
    let rt = comp.into_decompressed_bytes().unwrap();
    let fini_str = std::str::from_utf8(&rt).unwrap();
    assert_eq!(init_str, fini_str);
  }

  #[test]
  fn round_trip_offset_2() {
    let init_str = "Entire any had depend and figure winter. Change stairs and men likely wisdom new happen piqued six. Now taken him timed sex world get. Enjoyed married an feeling delight pursuit as offered. As admire roused length likely played pretty to no. Means had joy miles her merry solid order.";
    let data = init_str.to_owned().into_bytes();
    let comp = CompressedBlock::bytes_to_lz77_offset(&data);
    let rt = comp.into_decompressed_bytes().unwrap();
    let fini_str = std::str::from_utf8(&rt).unwrap();
    assert_eq!(init_str, fini_str);
  }

  #[test]
  fn simple_offset_backref() {
    let data = vec![
      0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    ];
    let mut match_table = HashMap::new();
    match_table.insert((1, 2), VecDeque::from_iter(vec![4, 2, 1, 0].into_iter()));

    let x = CompressedBlock::find_offset_backref(&mut match_table, &data, 4);
    println!("{:?}", x);
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
    ];
    assert_eq!(symstream, answer);
  }

  #[test]
  fn simple_offsetbr_roundtrip() {
    let data = vec![
      0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    ];
    let comp = CompressedBlock::bytes_to_lz77_offset(&data);
    let rt = comp.into_decompressed_bytes().unwrap();
    assert_eq!(data, rt);
  }

  #[test]
  fn round_trip_offset() {
    let mut testvec = vec![1,2,3,4,3,2,1];
    for i in 0..10 {
      testvec.push(i);
    }
    testvec.append(&mut vec![15,16,17,18,17,16,15]);
    let comp = CompressedBlock::bytes_to_lz77_offset(&testvec);
    println!("{:?}", comp.data);
    assert!(comp.data.iter().any(|x| match x {
      DeflateSym::OffsetBackref(_,_,_) => true,
      _ => false,
    }));
    let rt = comp.into_decompressed_bytes().unwrap();

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
    let mut testvec = vec![1,2,3,4,3,2,1];
    for _ in 0..100 {
      testvec.push(rng.gen::<u8>());
    }
    testvec.append(&mut vec![15,16,17,18,17,16,15]);
    let comp = CompressedBlock::bytes_to_lz77(&testvec);


    let rt = comp.into_decompressed_bytes().unwrap();


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
  fn round_trip_randomlike_offset(){
    let mut rng = rand::thread_rng();
    let mut testvec = vec![1,2,3,4,3,2,1];
    for _ in 0..100 {
      testvec.push(rng.gen::<u8>());
    }
    testvec.append(&mut vec![15,16,17,18,17,16,15]);
    let comp = CompressedBlock::bytes_to_lz77_offset(&testvec);

    assert!(comp.data.iter().any(|x| match x {
      DeflateSym::OffsetBackref(_,_,_) => true,
      _ => false,
    }));
    let rt = comp.into_decompressed_bytes().unwrap();

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
