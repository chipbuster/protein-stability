use core::cmp::{Eq, PartialEq};
use std::collections::VecDeque;
use std::iter::Iterator;

/** Sliding windows always occur on raw inputs, but it might be over larger
integer types in later experiments.
*/
pub struct SlidingWindow<'a, S> {
  source: &'a [S],
  // hashes: VecDeque<usize>,     // Do not implement until we can prove it's needed
  window: (usize, usize), // A right-open, i.e. [a, b), pair of indices
  winsz: usize,
}

impl<'a, S> SlidingWindow<'a, S>
where
  S: Copy + PartialEq + Eq,
{
  const DEFAULT_WIN_SIZE: usize = 1024;
  pub fn new(source: &'a [S]) -> Self {
    Self::new_with_params(source, Self::DEFAULT_WIN_SIZE)
  }

  pub fn new_with_params(source: &'a [S], winsz: usize) -> Self {
    let win_end = std::cmp::max(winsz, source.len());
    Self {
      source,
      window: (0, win_end),
      winsz,
    }
  }

  /// Add a new symbol into the sliding window, forcing the old one out if
  /// necessary.
  pub fn advance(&mut self) {
    if self.window.1 < self.source.len() {
      self.window = (self.window.0 + 1, self.window.1 + 1);
    }
  }

  /// Find the longest match in the current sliding window using a brute-force
  /// search algorithm.
  pub fn find_match(&self, to_match: &[S]) -> (usize, usize) {
    let mut backref_dist = 0;
    let mut backref_size = 0;

    // Start searching at beginning of sliding window.
    for i in 0..=self.winsz {
      let dist = self.winsz - i;
      let match_size = self.longest_match(i, to_match);
      if match_size > backref_size {
        backref_dist = i;
        backref_size = match_size;
      }
    }

    (backref_dist, backref_size)
  }

  /* Dirty index-based searches because Rust iterators are still too hard for
  my little peanut brain to understand. */
  fn longest_match(&self, win_index_start: usize, to_match: &[S]) -> usize {
    let mut offset = 0usize;
    while self.window[win_index_start + offset] == to_match[offset] {
      offset += 1;
    }
    offset
  }
}
