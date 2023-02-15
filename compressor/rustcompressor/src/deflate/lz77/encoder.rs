//! Contains the functions, types, and aliases necessary to do the nasty work of
/// actually generating lz77 streams (ve)
use crate::deflate::codepoints::DEFAULT_CODEPOINTS;
use crate::deflate::default_data::default_huffcode::DEFAULT_DIST_CODE;
use crate::deflate::{CodeDict, DeflateSym, LZSym};
use crate::huff_tree::huffcode_from_freqs;

use num::Bounded;
use static_assertions::const_assert_eq;

use std::collections::{HashMap, VecDeque};
use std::convert::{TryFrom, TryInto};
use std::hash::Hash;

type Dist = usize;
type Len = usize;
type Index = usize;

const MAX_HUFF_LEN: Option<usize> = Some(15);
const MAX_DEFLATE_MATCH_LEN: usize = 258;
const MIN_DEFLATE_MATCH_LEN: Len = 3;
const MAX_DEFLATE_MATCH_DIST: Dist = 32768;

type BackrefMap<S> = HashMap<S, VecDeque<Index>>;

/// Specifies the longest match length + distance that should be allowed when doing
/// LZ77.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct LZMaximums {
  max_length: Len,
  max_dist: Dist,
}

impl LZMaximums {
  pub fn new(max_length: Len, max_dist: Dist) -> Self {
    Self {
      max_length,
      max_dist,
    }
  }

  /// Get a MaxMatchParameter representing the largest possible length/distance
  /// used by the backing datatype
  pub fn max() -> Self {
    static_assertions::assert_type_eq_all!(Len, Dist, usize);
    Self {
      max_length: std::usize::MAX,
      max_dist: std::usize::MAX,
    }
  }
}

impl Default for LZMaximums {
  /// Get a MaxMatchParameter representing the largest possible length/distance
  /// used by DEFLATE as defined in RFC 1951
  fn default() -> Self {
    Self::new(MAX_DEFLATE_MATCH_LEN, MAX_DEFLATE_MATCH_DIST)
  }
}

/// A collection of arguments that controls how the
#[derive(Debug)]
pub struct LZRules {
  limits: LZMaximums,
}

impl LZRules {
  pub fn new(limits: LZMaximums) -> Self {
    Self { limits }
  }
}

/// Assuming a match of size `length-1` succeeded at this index, does a match of `length` succeed?
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

/// Given a match of length three at the given indices, attempt to expand this to the
/// longest possible match.
fn longest_match_at_index(
  data: &[u8],
  data_start: Index,
  match_start: Index,
  offset: u8,
  lzrules: &LZRules,
) -> Len {
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
  while len < lzrules.limits.max_length
    && check_match_valid(data, data_start, match_start, len + 1, offset)
  {
    len += 1;
  }
  len
}

/// Given a set of proposed (length-three) matches, do the following:
///  - Determine which of the given matches is actually the longest by attempting
///    to extend the match until that fails.
///  - Convert the indices of the longest match into a distance-length pair
///    that can be directly used in LZ77 compression
fn find_longest_match(
  data: &[u8],
  start: Index,
  match_indices: &[&Index],
  lzrules: &LZRules,
) -> (Dist, Len) {
  let mut max_len = 0usize;
  let mut max_index = 0usize;
  assert!(!match_indices.is_empty());

  for m in match_indices {
    let i = **m;
    let len = longest_match_at_index(data, start, i, 0, lzrules);
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

/// Find an offset backref for this location in the input. Returns a tuple
/// containing the symbol(s) to add to the output along with a usize indicating
/// how far to advance the input stream.
///
/// # Panics
///
/// Panics if the resulting match distance or length cannot be expressed in a
/// SizeTy. Caller is responsible for limiting match dist/length via LZRules
/// to avoid this.
fn find_backref<SizeTy, InitMatchTy>(
  match_table: &mut BackrefMap<InitMatchTy>,
  data: &[u8],
  index: usize,
  compute_init_match: impl Fn(&[u8], usize) -> InitMatchTy,
  lzrules: &LZRules,
) -> Option<(usize, Vec<LZSym<SizeTy>>)>
where
  SizeTy: TryFrom<usize>,
  InitMatchTy: std::fmt::Debug + PartialEq + Eq + Hash + Copy,
{
  /* This function contains the core logic for finding and extending a
  backreference using a BackrefMap. The InitMatchTy is the type of data structure
  used for the inital matching. Once an intial match has been found, it will be
  grown by the code to find the *longest* match at that position. RFC 1951
  suggests using a 3-byte array. In practice, this code will use short slices or
  tuples. The caller is responsible for making sure that the type stored in the
  match table and the type returned by the closure that computes match terms is
  the same (though the compiler ought to barf if they mismatch). */

  // Offset code assumes that this is 3: RefTy needs to change if this is wrong
  // Needs to be moved to before compute_match_term
  const_assert_eq!(MIN_DEFLATE_MATCH_LEN, 3);
  let max_dist = lzrules.limits.max_dist;

  // The DeflateSymbol output and how many raw bytes are represented by them, resp.
  let mut output = Vec::new();
  let mut n_consumed = 0usize;

  let term = compute_init_match(data, index);
  prune_matches(match_table, term, index, max_dist);
  let matches = get_matches(match_table, term, index);

  // No match. First, add this term into the match table so it shows up in future
  // searches. Then signal to the surrounding code that we were unable to match.
  if matches.is_empty() {
    match_table.entry(term).or_default().push_front(index);
    return None;
  }

  let (dist1, len1) = find_longest_match(data, index, &matches, lzrules);

  // We now have one reference we could emit for a backreference. However, RFC 1951
  // suggests a deferred matching procedure in which we try moving the window up
  // by one to see if we can find a longer match.
  // NB: technically not correct since we're looking a bit too far back (??)
  let term2 = compute_init_match(data, index + 1);
  prune_matches(match_table, term2, index + 1, max_dist);
  let matches2 = get_matches(match_table, term2, index);
  let (dist2, len2) = if !matches2.is_empty() {
    find_longest_match(data, index + 1, &matches2, lzrules)
  } else {
    (0, 0)
  };

  let (mdist, mlen, term, index) = if len2 > len1 {
    // Emit a literal and then the second match
    output.push(LZSym::Literal(data[index]));
    n_consumed += 1;
    (dist2, len2, term2, index + 1)
  } else {
    (dist1, len1, term, index)
  };

  n_consumed += mlen;
  match_table.entry(term).or_default().push_front(index);

  /* There is simply no way to recover from a conversion failure within this
  function. */
  let unwrap_convert = |x| match SizeTy::try_from(x) {
    Ok(val) => val,
    _ => panic!(
      "Failed to unwrap numeric type. Perhaps there was a mismatch
      between the maxmatchparameters and the size used for backrefs?"
    ),
  };

  debug_assert!(mdist <= max_dist);
  debug_assert_eq!(
    data[index - mdist..index - mdist + mlen],
    data[index..index + mlen]
  );
  output.push(LZSym::<SizeTy>::Backreference(
    unwrap_convert(mlen),
    unwrap_convert(mdist),
  ));

  Some((n_consumed, output))
}

/// Find a DEFLATE backref for this location in the input. These are the backrefs used
/// by DEFLATE: <length, distance> pairs. Returns a tuple containing the DEFLATE symbol(s)
/// to add to the output along with a usize indicating how far to advance the input stream.
fn find_deflate_backref<'a, SizeTy: TryFrom<usize>>(
  match_table: &mut BackrefMap<(u8, u8, u8)>,
  data: &'a [u8],
  index: usize,
  lzrules: &LZRules,
) -> (usize, Vec<LZSym<SizeTy>>) {
  let compute_match_stem = |d: &[u8], i: usize| (d[i], d[i + 1], d[i + 2]);

  let myrules = LZRules {
    limits: lzrules.limits,
  };

  let x = find_backref(match_table, data, index, compute_match_stem, &myrules);
  x.unwrap_or((1, vec![LZSym::Literal(data[index])]))
}

/// Collect a vec from the VecDeque in the backref map
fn get_matches<S: Hash + Eq>(backref_map: &BackrefMap<S>, term: S, index: usize) -> Vec<&Index> {
  match backref_map.get(&term) {
    None => Vec::new(),
    Some(x) => x
      .iter()
      .filter(|x| index - **x > MIN_DEFLATE_MATCH_LEN)
      .collect(),
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

/// Computes the compressed representation of a stream of bytes.
pub fn do_lz77<T>(data: &[u8], lzrules: &LZRules) -> Vec<LZSym<T>>
where
  T: TryFrom<usize> + TryInto<usize> + PartialOrd + Ord + PartialEq + Eq + Bounded,
{
  // Validate bounds in lzrules against limits on T to avoid panics within internal functions
  let t_max: usize = T::max_value().try_into().ok().unwrap();
  let t_min: usize = T::min_value().try_into().ok().unwrap();
  let valid_range = t_min..=t_max;
  assert!(valid_range.contains(&lzrules.limits.max_dist));
  assert!(valid_range.contains(&lzrules.limits.max_length));

  let mut deflate_match_table = HashMap::<(u8, u8, u8), VecDeque<Index>>::new();
  let mut offset_match_table = HashMap::<(u8, u8), VecDeque<Index>>::new();
  let mut output: Vec<LZSym<T>> = Vec::new();

  let mut index = 0usize;

  let mut checkpoints = if data.len() < 300_000usize {
    let mut x = Vec::new();
    x.push((data.len(), 100));
    let stepsz = data.len() / 10usize;
    for z in (0..9usize).rev() {
      x.push((stepsz * z, 10 * z));
    }
    x.push((data.len(), 100));
    x
  } else {
    let mut x = Vec::new();
    x.push((data.len(), 100));
    let stepsz = data.len() / 100usize;
    for z in (0..99usize).rev() {
      x.push((stepsz * z, z));
    }
    x
  };
  log::debug!(
    "Reporting at follow (#bytes, %complete) ranges: {:?}",
    checkpoints
  );

  // Main compression loop. Follow sections 4's suggestion and start by looking at triplets, then
  // expanding matches as needed. This loop does not guarantee that all input has been encoded
  // when it ends, hence the cleanup section afterwards.
  while index + MIN_DEFLATE_MATCH_LEN < data.len() {
    // Track progress using logging crate
    let (nbytes, percent) = checkpoints.last().copied().unwrap();
    if index > nbytes {
      log::debug!("Finished {}/{}, {}%", index, data.len(), percent);
      let _ = checkpoints.pop();
    }

    // The style here is not great: we re-use lzrules internally to tell the core
    // compression algorithm what we want to do, conflating its usage with whether
    // the user wants offset compression or not.

    let (deflate_len, deflate_syms) =
      find_deflate_backref(&mut deflate_match_table, &data, index, lzrules);

    // Select the match to use based on input options and match lengths
    let (match_len, mut match_syms) = (deflate_len, deflate_syms);

    index += match_len;
    output.append(&mut match_syms);
  }

  // Cleanup: emit the last few unconsumed elements as literals (if needed).
  while index < data.len() {
    output.push(LZSym::Literal(data[index]));
    index += 1
  }

  // Add an end-of-stream indicator
  output.push(LZSym::EndOfBlock);
  output
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
    }
  }
  freqs
}

fn compute_dist_freq(data: &[DeflateSym]) -> HashMap<u16, usize> {
  let mut freqs = HashMap::new();
  for x in data.iter() {
    match x {
      DeflateSym::Literal(_) | DeflateSym::EndOfBlock => continue,
      DeflateSym::Backreference(_, dist) => {
        let dc = &DEFAULT_CODEPOINTS.get_codepoint_for_dist(*dist).code();
        *freqs.entry(*dc).or_default() += 1;
      }
    }
  }
  freqs
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
  #[allow(unused_imports)]
  use super::*;
  use std::ops::Not;

  #[test]
  fn check_unbounded_max_dist() {
    // Ensure that when using u64-sized LZSyms, we can backref past the u16 limit.
    // This test may use tens of MB of memory.
    const MATCH_SYM: u8 = 7;
    const NOMATCH_SYM: u8 = 27;
    const MATCH_LEN: usize = 750;
    const MATCH_DIST: usize = 1 << 22;

    let mut data = Vec::new();
    data.append(&mut vec![MATCH_SYM; MATCH_LEN]);
    data.append(&mut vec![NOMATCH_SYM; MATCH_DIST]);
    data.append(&mut vec![MATCH_SYM; MATCH_LEN]);

    let rules = LZRules::new(LZMaximums::max());
    let syms = do_lz77::<usize>(&data, &rules);

    // Find a backref within syms that exceeds the default length/distance.
    assert!(
      syms
        .iter()
        .filter(|x| match x {
          LZSym::Backreference(l, d) => *l > MAX_DEFLATE_MATCH_LEN && *d > MAX_DEFLATE_MATCH_DIST,
          _ => false,
        })
        .collect::<Vec<_>>()
        .is_empty()
        .not(),
      "LZ77 could not find a match longer than the default lengths"
    );

    let u16_max = usize::from(u16::MAX);
    assert!(
      syms
        .iter()
        .filter(|x| match x {
          LZSym::Backreference(l, d) => *l > u16_max && *d > u16_max,
          _ => false,
        })
        .collect::<Vec<_>>()
        .is_empty()
        .not(),
      "LZ77 could not find a match longer than the default lengths"
    );
  }
}
