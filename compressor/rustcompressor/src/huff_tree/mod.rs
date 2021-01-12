/*! A Huffman tree implementation designed for use with DEFLATE-style trees,
and with the implementations in the bitstream_io library

Huffman Trees in the DEFLATE library carry additional constraints that
 - Symbols with the same length are listed in lexicographical order
 - Shorter codes lexicographically precede longer codes.

This makes it possible to specify a huffman tree by simply specifying the code
length for each symbol.
*/

use std::collections::HashMap;
use std::vec::Vec;

use std::cmp::Eq;
use std::cmp::Ord;
use std::fmt::Debug;
use std::hash::Hash;

use bit_vec::BitVec;

/// Convert the lower `nbits` bits of `source` into a bitvector in little-endian
/// packing
fn to_bitvec(nbits: usize, source: u64) -> BitVec {
  let mut mask = 1 << (nbits - 1);
  let mut v = BitVec::new();
  while mask != 0 {
    let elem = mask & source != 0;
    v.push(elem);
    mask >>= 1;
  }
  v
}

/// Transforms a vector of bits into a vector of 0-1 u8s. Useful for interfacing with the
/// bitstream-io crate.
fn bitvec_to_bytes(v: &BitVec) -> Vec<u8> {
  let mut out = Vec::new();
  for b in v.into_iter() {
    out.push(b as u8);
  }
  out
}

/// Given the code lengths per symbol, compute a canonical huffman code
pub fn huffcode_from_lengths<S>(codelens: &HashMap<S, usize>) -> Vec<(S, Vec<u8>)>
where
  S: Eq + PartialEq + Hash + PartialOrd + Ord + Clone + Debug,
{
  // Count the number of symbols with a given codelength
  let mut bl_count = HashMap::<usize, usize>::new();
  bl_count.insert(0, 0);
  for ct in codelens.values() {
    *bl_count.entry(*ct).or_insert(0) += 1;
  }
  let max_bits = *bl_count.keys().max().expect("No bitlength counts");
  for (size, count) in bl_count.iter() {
    if size == &0 {
      continue;
    }
    let max = 1usize << (size);
    // This is actually not restrictive enough since we can't actually use all
    // 2^n codes, but it's a good fail-safe check
    if count > &max {
      panic!(
        "There are {} codes that are {} bits long! Collision imminent.",
        count, size
      )
    }
  }

  // Compute the smallest code for each codelength
  let mut code = 0u64;
  let mut bl_code = HashMap::<usize, u64>::new();
  for bits in 1..=max_bits {
    let num_small = *bl_count.entry(bits - 1).or_default() as u64;
    code = (code + num_small) << 1;
    bl_code.insert(bits, code);
  }

  let mut result = Vec::new();
  let mut syms_to_encode = codelens.keys().cloned().collect::<Vec<S>>();
  syms_to_encode.sort();
  for sym in syms_to_encode.into_iter() {
    let sym_codelength = *codelens.get(&sym).unwrap();
    if sym_codelength == 0 {
      continue;
    }

    let sym_code = bl_code.get_mut(&sym_codelength).unwrap();
    result.push((sym, to_bitvec(sym_codelength, *sym_code)));
    *sym_code += 1;
  }

  let mut output = Vec::with_capacity(result.len());
  for (j, bitvec) in result.into_iter() {
    output.push((j, bitvec_to_bytes(&bitvec)));
  }

  output
}

// Given a set of code frequencies, compute the canonical huffman coding. Optionally
// can limit the maximum codelength used.
pub fn huffcode_from_freqs<S>(
  codefreqs: &HashMap<S, usize>,
  maxlen_inp: Option<usize>,
) -> Vec<(S, Vec<u8>)>
where
  S: Eq + PartialEq + Hash + PartialOrd + Ord + Clone + Debug + Copy,
{
  if codefreqs.is_empty() {
    return Vec::new();
  }

  let maxlen = maxlen_inp.unwrap_or(usize::MAX);
  let codelens = if codefreqs.len() == 1 {
    // A stupid workaround for the package-merge algorithm creating blanks
    // if fed a singleton. Fix this later.
    let k1: Vec<&S> = codefreqs.keys().collect();
    let k = *k1[0];
    let v = 1;
    let mut x = HashMap::new();
    x.insert(k, v);
    x
  } else {
    get_codeslens_restricted(&codefreqs, maxlen)
  };
  huffcode_from_lengths(&codelens)
}

/// Generate the restricted-length Huffman code for the given input.
pub fn huffcode_from_input<S>(input: &[S], maxlen_inp: Option<usize>) -> Vec<(S, Vec<u8>)>
where
  S: Eq + Hash + Clone + Copy + Ord + Debug,
{
  let maxlen = maxlen_inp.unwrap_or(usize::MAX);
  let freqs = get_frequencies(input);
  let codelens = get_codeslens_restricted(&freqs, maxlen);
  huffcode_from_lengths(&codelens)
}

/// Get the frequencies of characters in a given input
fn get_frequencies<S: Eq + Hash + Clone>(src: &[S]) -> HashMap<S, usize> {
  let mut counter: HashMap<S, usize> = HashMap::new();
  for ch in src.iter() {
    let chcount = counter.entry(ch.clone()).or_insert(0);
    *chcount += 1;
  }
  if counter.len() <= 1 {
    panic! {"Input is blank or has only one character."}
  }
  counter
}

#[derive(Clone, Copy, Default, Debug, Eq, PartialEq)]
struct Coin<S> {
  value: usize,
  invdenom: usize, // Denomination = 2^{-invdenom}
  sym: S,
}

#[derive(Clone, Default, Debug, Eq, PartialEq)]
struct CoinPackage<S> {
  value: usize,
  invdenom: usize,
  coins: Vec<Coin<S>>,
}

impl<S: Clone + Copy> CoinPackage<S> {
  fn new_singleton(c: Coin<S>) -> Self {
    Self {
      value: c.value,
      invdenom: c.invdenom,
      coins: vec![c],
    }
  }

  fn merge(self, other: CoinPackage<S>) -> CoinPackage<S> {
    assert!(other.invdenom == self.invdenom);
    assert!(other.invdenom != 0, "Merging two max-value packages!");
    let value = self.value + other.value;
    let invdenom = self.invdenom - 1;
    let coins = self
      .coins
      .into_iter()
      .chain(other.coins.into_iter())
      .collect();
    Self {
      value,
      invdenom,
      coins,
    }
  }
}

fn coin_merge<S: Clone + Copy + Eq>(mut v: Vec<CoinPackage<S>>) -> Vec<CoinPackage<S>> {
  let mut out = Vec::new();
  v.sort_by(|c1, c2| c1.value.cmp(&c2.value));

  let mut i = 0usize;
  while i + 1 < v.len() {
    out.push(v[i].clone().merge(v[i + 1].clone()));
    i += 2;
  }

  out
}

/// Build a huffman tree with a restricted maximum code length using package-merge
fn get_codeslens_restricted<S>(freqs: &HashMap<S, usize>, maxlen: usize) -> HashMap<S, usize>
where
  S: Eq + Hash + Ord + Clone + Copy,
{
  let n = freqs.len();
  let mut syms = Vec::with_capacity(n);
  for (sym, freq) in freqs.iter() {
    syms.push((*sym, *freq))
  }
  syms.sort_by(|(_, y1), (_, y2)| y1.cmp(y2));

  let mut coins = Vec::new();
  for (sym, freq) in syms {
    for j in 1..=maxlen {
      coins.push(Coin {
        value: freq,
        invdenom: j,
        sym,
      });
    }
  }

  // Execute the package-merge algorithm to obtain the min-value set of denom n-1
  let mut out = package_merge(coins);
  let mut rescoins: Vec<Coin<S>> = Vec::new();
  for coincollection in out.iter_mut().take(n-1) {
    assert_eq!(coincollection.invdenom, 0);
    rescoins.append(&mut coincollection.coins);
  }

  let mut code_lens = HashMap::new();
  for coin in rescoins {
    let id = coin.sym;
    *code_lens.entry(id).or_default() += 1;
  }
  code_lens
}

/// Execute package merge on a set of coins. Assumes that there is at least one coin of each
/// denomination between the largest and smallest denominations (i.e. no denom is skipped)
fn package_merge<S: Clone + Copy + Eq>(init_coins: Vec<Coin<S>>) -> Vec<CoinPackage<S>> {
  let mut coinmap: HashMap<usize, Vec<CoinPackage<S>>> = HashMap::new();
  for coin in init_coins.into_iter() {
    coinmap
      .entry(coin.invdenom)
      .or_default()
      .push(CoinPackage::new_singleton(coin));
  }

  let mut coinpacs: Vec<Vec<CoinPackage<S>>> = Vec::new();
  let ndenom = *coinmap.keys().max().unwrap() + 1;
  coinpacs.resize_with(ndenom, Default::default);
  for (i, pac) in coinmap.into_iter() {
    coinpacs[i] = pac;
  }

  // Merge procedure
  while coinpacs.len() > 1 {
    let to_merge = coinpacs.pop().unwrap();
    let mut merged = coin_merge(to_merge);
    let l = coinpacs.len();
    coinpacs[l - 1].append(&mut merged);
  }

  assert_eq!(coinpacs.len(), 1);
  coinpacs[0].clone()
}

mod tests {
  #[allow(unused_imports)]
  use super::*;

  #[test]
  fn simple_hufftree_test() {
    let x = vec![
      (0, 3),
      (1, 3),
      (2, 3),
      (3, 3),
      (4, 3),
      (5, 2),
      (6, 4),
      (7, 4),
    ];
    let mut codelens = HashMap::new();
    for (k, v) in x.into_iter() {
      codelens.insert(k, v);
    }
    let z: Vec<(i32, Vec<u8>)> = huffcode_from_lengths(&codelens);
    for (sym, code) in z {
      let truecode = match sym {
        0 => vec![0, 1, 0],
        1 => vec![0, 1, 1],
        2 => vec![1, 0, 0],
        3 => vec![1, 0, 1],
        4 => vec![1, 1, 0],
        5 => vec![0, 0],
        6 => vec![1, 1, 1, 0],
        7 => vec![1, 1, 1, 1],
        _ => panic!("Code not in input test code--test is broken!"),
      };
      assert_eq!(truecode, code);
    }
  }

  #[cfg(test)]
  fn increment_bit_slice(mut arg: Vec<u8>) -> Option<Vec<u8>> {
    let mut carry = 1u8;
    for x in arg.iter_mut().rev() {
      match (*x, carry) {
        (0, 0) => carry = 0,
        (0, 1) => {
          carry = 0;
          *x = 1;
        }
        (1, 0) => {}
        (1, 1) => {
          *x = 0;
          carry = 1
        }
        _ => return None,
      }
    }
    Some(arg)
  }

  #[cfg(test)]
  fn default_hufftree_values() -> Vec<(u16, Vec<u8>)> {
    let mut huff_values = Vec::with_capacity(288);

    // Code Block 1: 00110000 through 10111111 for values 0-143
    let mut code = Some(vec![0u8, 0, 1, 1, 0, 0, 0, 0]);
    for val in 0..=143u16 {
      let code2 = code.as_ref().unwrap().clone();
      huff_values.push((val, code2));
      code = increment_bit_slice(code.unwrap());
    }

    // Code Block 2: 110010000 through 111111111 for values 144-255
    let mut code = Some(vec![1u8, 1, 0, 0, 1, 0, 0, 0, 0]);
    for val in 144..=255u16 {
      let code2 = code.as_ref().unwrap().clone();
      huff_values.push((val, code2));
      code = increment_bit_slice(code.unwrap());
    }

    // Code Block 3: 0000000 through 0010111 for values 256-279
    let mut code = Some(vec![0u8, 0, 0, 0, 0, 0, 0]);
    for val in 256..=279u16 {
      let code2 = code.as_ref().unwrap().clone();
      huff_values.push((val, code2));
      code = increment_bit_slice(code.unwrap());
    }

    // Code Block 4: 11000000 through 11000111 for 280-287
    let mut code = Some(vec![1u8, 1, 0, 0, 0, 0, 0, 0]);
    for val in 280..=287u16 {
      let code2 = code.as_ref().unwrap().clone();
      huff_values.push((val, code2));
      code = increment_bit_slice(code.unwrap());
    }
    huff_values
  }

  #[test]
  fn default_hufftree_test() {
    let mut codelens = HashMap::<u16, usize>::new();
    for j in 0..=287 {
      match j {
        0..=143 => codelens.insert(j, 8),
        144..=255 => codelens.insert(j, 9),
        256..=279 => codelens.insert(j, 7),
        280..=287 => codelens.insert(j, 8),
        _ => None,
      };
    }
    let rawcodes = huffcode_from_lengths(&codelens);
    assert_eq!(rawcodes, default_hufftree_values());
  }

  #[test]
  fn test_coin_merge() {
    let x = vec![
      Coin {
        value: 8,
        invdenom: 3,
        sym: 1,
      },
      Coin {
        value: 3,
        invdenom: 3,
        sym: 2,
      },
      Coin {
        value: 5,
        invdenom: 3,
        sym: 3,
      },
      Coin {
        value: 6,
        invdenom: 3,
        sym: 4,
      },
      Coin {
        value: 2,
        invdenom: 3,
        sym: 5,
      },
    ];

    let q = x
      .into_iter()
      .map(|x| CoinPackage::new_singleton(x))
      .collect();
    let y = coin_merge(q);

    let r = vec![
      CoinPackage {
        value: 5,
        invdenom: 2,
        coins: vec![
          Coin {
            value: 2,
            invdenom: 3,
            sym: 5,
          },
          Coin {
            value: 3,
            invdenom: 3,
            sym: 2,
          },
        ],
      },
      CoinPackage {
        value: 11,
        invdenom: 2,
        coins: vec![
          Coin {
            value: 5,
            invdenom: 3,
            sym: 3,
          },
          Coin {
            value: 6,
            invdenom: 3,
            sym: 4,
          },
        ],
      },
    ];

    println!("Output: {:#?}", y);
    println!("Reference: {:#?}", r);

    assert_eq!(y, r);
  }

  #[test]
  fn test_codelength_gen() {
    let freqs: HashMap<u8, usize> = [(1, 1), (2, 32), (3, 16), (4, 4), (5, 8), (6, 2), (7, 1)]
      .iter()
      .cloned()
      .collect();
    let max8: HashMap<u8, usize> = [(1, 6), (2, 1), (3, 2), (4, 4), (5, 3), (6, 5), (7, 6)]
      .iter()
      .cloned()
      .collect();
    let max5: HashMap<u8, usize> = [(1, 5), (2, 1), (3, 2), (4, 5), (5, 3), (6, 5), (7, 5)]
      .iter()
      .cloned()
      .collect();
    let sizes1 = get_codeslens_restricted(&freqs, 8);
    let sizes2 = get_codeslens_restricted(&freqs, 5);
    assert_eq!(sizes1, max8);
    assert_eq!(sizes2, max5);
  }
}
