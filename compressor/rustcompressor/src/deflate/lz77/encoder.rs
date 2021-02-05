//! Contains the functions, types, and aliases necessary to do the nasty work of
/// actually generating lz77 streams (ve)
use crate::deflate::codepoints::{DEFAULT_CODEPOINTS, OFFSET_SIGIL};
use crate::deflate::default_data::default_huffcode::DEFAULT_DIST_CODE;
use crate::deflate::{CodeDict, CompressedBlock, DeflateSym};
use crate::huff_tree::huffcode_from_freqs;

use std::collections::{HashMap, VecDeque};
use std::hash::Hash;

type Dist = usize;
type Len = usize;
type Index = usize;

const MAX_HUFF_LEN: Option<usize> = Some(15);
const MAX_LZ_LEN: usize = 258;
const CHUNK_SZ: Len = 3;
const MAX_MATCH_DIST: Dist = 16384;

type BackrefMap<S> = HashMap<S, VecDeque<Index>>;

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
  if !no_data_overrun || !match_no_overlap {
    return false;
  }
  data[data_start + length - 1] == data[match_start + length - 1].wrapping_add(offset)
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
  while len < MAX_LZ_LEN && check_match_valid(data, data_start, match_start, len + 1, offset) {
    len += 1;
  }
  len
}

fn find_longest_match(
  data: &[u8],
  start: Index,
  match_indices: &[&Index],
  use_offset: bool,
) -> (Dist, Len) {
  let mut max_len = 0usize;
  let mut max_index = 0usize;
  assert!(!match_indices.is_empty());
  for m in match_indices {
    let i = **m;
    let len = if use_offset {
      let offset = data[start].wrapping_sub(data[i]);
      longest_match_at_index(data, start, i, offset)
    } else {
      longest_match_at_index(data, start, i, 0)
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
fn find_offset_backref(
  match_table: &mut BackrefMap<(u8, u8)>,
  data: &[u8],
  index: usize,
) -> Option<(usize, Vec<DeflateSym>)> {
  // Note: if CHUNK_SZ ever changes from 3, we need to change the type of match_table
  assert_eq!(CHUNK_SZ, 3);

  let base = data[index];
  let term = (
    data[index + 1].wrapping_sub(base),
    data[index + 2].wrapping_sub(base),
  );

  let mut output = Vec::new();
  let mut n_consumed = 0usize;

  prune_matches(match_table, term, index, MAX_MATCH_DIST);
  let matches = get_matches(match_table, term, index);

  // We don't *have* to find a match for offset backrefs (because they're an optional feature).
  // Therefore, we return None to signal that no match could be found and let the DEFLATE backref
  // function deal with the emitting of literal single bytes.
  if matches.is_empty() {
    match_table.entry(term).or_default().push_front(index);
    return None;
  };

  let (dist1, len1) = find_longest_match(data, index, &matches, true);

  // Search at index = index + 1 for a better match
  let term2 = (
    data[index + 2].wrapping_sub(data[index + 1]),
    data[index + 3].wrapping_sub(data[index + 1]),
  );

  prune_matches(match_table, term2, index + 1, MAX_MATCH_DIST);
  let matches2 = get_matches(&match_table, term2, index);
  let (dist2, len2) = if !matches2.is_empty() {
    find_longest_match(data, index + 1, &matches2, true)
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
  assert!(mdist <= MAX_MATCH_DIST);

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
  let term = &data[index..index + CHUNK_SZ];

  prune_matches(match_table, term, index, MAX_MATCH_DIST);
  let matches = get_matches(&match_table, term, index);

  // If no match, add this entry to the match table and emit a literal symbol
  if matches.is_empty() {
    match_table.entry(term).or_default().push_front(index);
    output.push(DeflateSym::Literal(data[index]));
    return (1, output);
  }

  // Find longest match starting at this index
  let (dist1, len1) = find_longest_match(data, index, &matches, false);

  // Follow the deflate suggestion about deferred matching
  // This is technically wrong since we're looking back too far by 1, but
  // since our max match isn't the DEFLATE maximum, it's a non-issue atm.
  let search_term_2 = &data[index + 1..index + 1 + CHUNK_SZ];
  prune_matches(match_table, search_term_2, index + 1, MAX_MATCH_DIST);
  let matches2 = get_matches(&match_table, search_term_2, index);
  let (dist2, len2) = if !matches2.is_empty() {
    find_longest_match(data, index + 1, &matches2, false)
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

  assert!(data[index - mdist..index - mdist + mlen] == data[index..index + mlen]);
  assert!(mdist <= MAX_MATCH_DIST);
  match_table.entry(term).or_default().push_front(index);

  n_consumed += mlen;
  output.push(DeflateSym::Backreference(mlen as u16, mdist as u16));

  (n_consumed, output)
}

/// Collect a vec from the VecDeque in the backref map
fn get_matches<S: Hash + Eq>(backref_map: &BackrefMap<S>, term: S, index: usize) -> Vec<&Index> {
  match backref_map.get(&term) {
    None => Vec::new(),
    Some(x) => x.iter().filter(|x| index - **x > CHUNK_SZ).collect(),
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

fn compute_lengthlit_freq(data: &[DeflateSym]) -> HashMap<u16, usize> {
  let mut freqs = HashMap::new();
  for x in data.iter() {
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

fn compute_dist_freq(data: &[DeflateSym]) -> HashMap<u16, usize> {
  let mut freqs = HashMap::new();
  for x in data.iter() {
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

/// Computes the compressed representation of a stream of bytes as well as the
/// huffman trees that will be used to encode it in DEFLATE
pub fn do_lz77(data: &[u8], use_offset: bool) -> Vec<DeflateSym> {
  let mut deflate_match_table = HashMap::<&[u8], VecDeque<Index>>::new();
  let mut offset_match_table = HashMap::<(u8, u8), VecDeque<Index>>::new();
  let mut output: Vec<DeflateSym> = Vec::new();

  let mut index = 0usize;

  // Main compression loop. Follow sections 4's suggestion and start by looking at triplets, then
  // expanding matches as needed. This loop does not guarantee that all input has been encoded
  // when it ends, hence the cleanup section afterwards.
  while index + CHUNK_SZ < data.len() {
    let (deflate_len, deflate_syms) = find_deflate_backref(&mut deflate_match_table, &data, index);
    let offset_match = if use_offset {
      find_offset_backref(&mut offset_match_table, &data, index)
    } else {
      None
    };

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
  output
}

/// Compute the lengthlit and dist codes that will be needed for a given input.
pub fn compute_codes(data: &[DeflateSym]) -> (CodeDict<u16>, CodeDict<u16>) {
  let lit_freq = compute_lengthlit_freq(data);
  let length_codes = huffcode_from_freqs(&lit_freq, MAX_HUFF_LEN);
  /* If we have distances in the LZ77 encoding, use them to describe a dict.
  Otherwise, it doesn't really matter what we use, so it might as well
  be the default dictionary */
  let dist_freq = compute_dist_freq(data);
  let dist_codes = if dist_freq.is_empty() {
    DEFAULT_DIST_CODE.clone()
  } else {
    huffcode_from_freqs(&dist_freq, MAX_HUFF_LEN)
  };
  (length_codes, dist_codes)
}

// More tests for encoding can be found in deflate::encoder
#[cfg(test)]
mod tests {
  use std::iter::FromIterator;

  #[allow(unused_imports)]
  use super::*;
  use rand::Rng;
  #[test]
  fn simple_offset_backref() {
    let data = vec![
      0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    ];
    let mut match_table = HashMap::new();
    match_table.insert((1, 2), VecDeque::from_iter(vec![4, 2, 1, 0].into_iter()));

    find_offset_backref(&mut match_table, &data, 4)
      .expect("Did not find offset in an input where there should have been one!");
  }
}
