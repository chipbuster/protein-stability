/*! The a dynamically-encoded DEFLATE stream block is preceded by a header which
contains the following information:

  - 5 bits: HLIT, # of length/literal codes - 257
  - 5 bits: HDIST, # of distance codes - 1
  - 4 bits: HCLEN, # of Code Length codes - 4
  - (HCLEN + 4) * 3 bits of code lengths for the code alphabet specified by HCLEN,
    given in the order 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15

  HLIT + 257 code lengths, encoded using the code length alphabet
  HDIST + 1 distance lengths, encoded with the code length alphabet
  From these code lengths, it is possible to construct the canonical Huffman
  codes used for the distance and length/lit trees, and thus to decode the
  DEFLATE payload.

  This module contains code for representing, encoding, and decoding these
  headers + codes. The top level functions (name) and (name), simply convert
  between a length/lit + distance tree and a corresponding bit representation
  in the appropriate bit source/sink. Most of the other functions in this module
  are utility functions.
*/

/* From RFC 1951:

The Huffman codes for the two alphabets appear in the block
immediately after the header bits and before the actual
compressed data, first the literal/length code and then the
distance code.  Each code is defined by a sequence of code
lengths, as discussed in Paragraph 3.2.2, above.  For even
greater compactness, the code length sequences themselves are
compressed using a Huffman code.  The alphabet for code lengths
is as follows:

      0 - 15: Represent code lengths of 0 - 15
          16: Copy the previous code length 3 - 6 times.
              The next 2 bits indicate repeat length
                    (0 = 3, ... , 3 = 6)
                 Example:  Codes 8, 16 (+2 bits 11),
                           16 (+2 bits 10) will expand to
                           12 code lengths of 8 (1 + 6 + 5)
          17: Repeat a code length of 0 for 3 - 10 times.
              (3 bits of length)
          18: Repeat a code length of 0 for 11 - 138 times
              (7 bits of length)

A code length of 0 indicates that the corresponding symbol in
the literal/length or distance alphabet will not occur in the
block, and should not participate in the Huffman code
construction algorithm given earlier.  If only one distance
code is used, it is encoded using one bit, not zero bits; in
this case there is a single code length of one, with one unused
code.  One distance code of zero bits means that there are no
distance codes used at all (the data is all literals).

*/

use bitstream_io::huffman::{compile_read_tree, compile_write_tree};
use bitstream_io::{BitReader, BitWriter, LittleEndian};
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::convert::TryInto;
use std::io::{Read, Write};
use std::vec::Vec;

use super::{CodeDict, DeflateReadTree, DeflateSym, DeflateWriteTree};
use crate::deflate::decoder::DeflateReadError;
use crate::deflate::encoder::DeflateWriteError;
use crate::huff_tree::*;

lazy_static! {
  static ref RAW_CODE_ORDER: Vec<u16> =
    vec![16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15,];
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum CodeLengthCodepoint {
  Length(u8),       // A Number of bits, 0-15
  ValueRepeat(u8),  // Valid values: 3-6
  ShortZeroRep(u8), // Valid values: 3-10
  LongZeroRep(u8),  // Valid values: 11-138
}

/// Read a DEFLATE header from stream and return the length/lit and distance trees
pub fn read_header<R: Read>(
  bit_src: &mut BitReader<R, LittleEndian>,
) -> Result<(Box<[DeflateReadTree]>, Box<[DeflateReadTree]>), DeflateReadError> {
  let hlit: u8 = bit_src.read(5)?;
  let hdist: u8 = bit_src.read(5)?;
  let hclen: u8 = bit_src.read(4)?;
  let size_codes = read_size_codes(bit_src, hclen + 4)?;
  println!("hlit: {}",hlit);
  println!("hdist: {}",hdist);
  println!("hclen: {}",hclen);
  println!("size codes: {:?}", size_codes);

  let size_code_tree = compile_read_tree(size_codes).expect("Couldn't compile read tree");

  let num_literals = 257 + hlit as u16;
  let num_dists = 1 + hdist as u16;

  let (length_tree, dist_tree) =
    decode_huffman_alphabets(bit_src, &size_code_tree, num_literals, num_dists)?;

  Ok((length_tree, dist_tree))
}

// TODO: Test function!
// TODO: Modify decoder to use read_header and then make other functions in here private.
// TODO: Modify read_header to return codedicts instead of precompiled trees.
pub fn write_header<W: Write>(
  bit_sink: &mut BitWriter<W, LittleEndian>,
  length_code: &CodeDict<u16>,
  dist_code: &CodeDict<u16>,
) -> Result<(), DeflateWriteError> {
  let length_dict: HashMap<u16, Vec<u8>> = length_code.into_iter().cloned().collect();
  let dist_dict: HashMap<u16, Vec<u8>> = dist_code.into_iter().cloned().collect();

  let largest_length_code = length_code.iter().map(|(x, _)| *x).max().unwrap_or(0);
  let largest_dist_code = dist_code.iter().map(|(x, _)| *x).max().unwrap_or(0);

  // If the length code doesn't have an encoding for 256, we can't end the stream, which is bad.
  assert!(largest_length_code >= 256);

  // Get the set of all code lengths and build a codepoint representation for it
  let mut code_sizes = Vec::new();
  for i in 0..=largest_length_code {
    code_sizes.push(
      length_dict
        .get(&i)
        .map(|x| x.len().try_into().unwrap())
        .unwrap_or(0),
    );
  }
  for i in 0..=largest_dist_code {
    code_sizes.push(
      dist_dict
        .get(&i)
        .map(|x| x.len().try_into().unwrap())
        .unwrap_or(0),
    );
  }
  let codepoints = code_lengths_to_codepoints(&code_sizes[..]);

  // Build the frequency map for codepoints.
  let mut codepoint_freqs: HashMap<u16, usize> = HashMap::new();
  for c in codepoints.iter() {
    match c {
      CodeLengthCodepoint::Length(n) => *codepoint_freqs.entry(*n as u16).or_insert(0usize) += 1,
      CodeLengthCodepoint::ValueRepeat(_) => *codepoint_freqs.entry(16).or_insert(0usize) += 1,
      CodeLengthCodepoint::ShortZeroRep(_) => *codepoint_freqs.entry(17).or_insert(0usize) += 1,
      CodeLengthCodepoint::LongZeroRep(_) => *codepoint_freqs.entry(18).or_insert(0usize) += 1,
    }
  }
  let size_codes = huffcode_from_freqs(&codepoint_freqs, Some(7));
  let size_codes_hm = size_codes
    .iter()
    .cloned()
    .collect();

  // Find index of last code length that will be used from RAW_CODE_ORDER
  let mut uniq_codelen_sizes:Vec<usize> = size_codes.iter().map(|(_,code)| code.len()).collect();
  uniq_codelen_sizes.sort();
  uniq_codelen_sizes.dedup();
  let mut last_rco_index = 3u16;
  let mut index = 3u16;
  while index < RAW_CODE_ORDER.len().try_into().unwrap() {
    let len = &RAW_CODE_ORDER[index as usize];
    if uniq_codelen_sizes.iter().any(|x| *x as u16  == *len) {
      last_rco_index = index.try_into().expect("RCO Array is somehow larger than u16::MAX");
    }
    index += 1;
  }
  
  /* These values come from the RFC 1951 rules (see module docs). We dont' check
  the max range because it differs based on offset-encodings and will be panic-ed
  on if it exceeds the range of bits in the BitWriter */
  let hlit = largest_length_code + 1 - 257;
  let hdist = largest_dist_code + 1 - 1;
  let hclen = last_rco_index + 1 - 4;
  

  bit_sink.write(5, hlit)?;
  bit_sink.write(5, hdist)?;
  bit_sink.write(4, hclen)?;
  write_size_codes(bit_sink, &size_codes_hm, hclen + 4)?;

  println!("hlit: {}", hlit);
  println!("hdist: {}", hdist);
  println!("hclen: {}", hclen);
  println!("{:?}", size_codes);

  let write_tree = compile_write_tree(size_codes).expect("Could not compute write tree");

  assert_eq!((hlit + 257 + hdist + 1) as usize, code_sizes.len());
  encode_huffman_alphabets(bit_sink, &write_tree, &code_sizes[..])?;

  Ok(())
}

/// Unpack and interpret the HCLEN + 4 encoding, returning a huffman tree that
/// can be used to decode the code lengths in the primary DEFLATE Huffman tree
pub fn read_size_codes<R: Read>(
  bit_src: &mut BitReader<R, LittleEndian>,
  num_codes: u8,
) -> Result<Vec<(u16, Vec<u8>)>, DeflateReadError> {
  let nc = num_codes as usize;

  let mut codecodelen = HashMap::new();
  let codes = &RAW_CODE_ORDER[0..nc];
  for code in codes.into_iter() {
    let ccl: usize = bit_src.read::<u8>(3)? as usize;
    codecodelen.insert(*code, ccl);
  }

  Ok(huffcode_from_lengths(&codecodelen))
}

/// Takes a code dictionary used to encode the code lengths in the primary DEFLATE
/// tree and writes the HCLEN + 4 bits header that defines that code length encoding.
/// size_codes should encode the integers 0-longest_code + 16,17,18.
fn write_size_codes<W: Write>(
  bit_sink: &mut BitWriter<W, LittleEndian>,
  size_codes: &HashMap<u16, Vec<u8>>,
  num_codes: u16,
) -> Result<(), DeflateWriteError> {
  let num_codes = num_codes.try_into().unwrap();
  for code_val in &RAW_CODE_ORDER[0..num_codes] {
    if let Some(codelen) = size_codes.get(code_val).and_then(|x| Some(x.len())) {
      assert!(codelen < 8);
      println!("Wrote code length {} encoded as {}", code_val, codelen);
      bit_sink.write(3, codelen as u8)?;
    } else {
      bit_sink.write(3, 0)?;
      println!("Wrote code length {} encoded as {}", code_val, 0);
    }
  }
  Ok(())
}

pub fn encode_huffman_alphabets<W: Write>(
  bit_sink: &mut BitWriter<W, LittleEndian>,
  size_huffman: &DeflateWriteTree,
  code_sizes: &[u8],
) -> Result<(), DeflateWriteError> {
  for cs in code_sizes {
    let c = *cs as u16;
    bit_sink.write_huffman(size_huffman, c)?;
  }
  Ok(())
}

pub fn decode_huffman_alphabets<R: Read>(
  bit_src: &mut BitReader<R, LittleEndian>,
  size_huffman: &[DeflateReadTree],
  num_literals: u16,
  num_distances: u16,
) -> Result<(Box<[DeflateReadTree]>, Box<[DeflateReadTree]>), DeflateReadError> {
  let num_symbols = (num_literals + num_distances).try_into().unwrap();
  let mut codes = Vec::new();
  let mut lengths = Some(Vec::new());

  while lengths.is_some() && lengths.as_ref().unwrap().len() < num_symbols {
    codes.push(CodeLengthCodepoint::read_from_bitstream(
      bit_src,
      size_huffman,
    )?);
    lengths = codepoints_to_code_lengths(&codes[..]);
  }

  let lengths = lengths.unwrap();
  assert!(num_distances < num_literals);
  assert_eq!(lengths.len(), (num_literals + num_distances) as usize);
  // We now have the codelengths for distances and sizes. Get Huffman Code by
  // generating appropriate hashmaps;
  let mut literal_lengths = HashMap::<u16, usize>::new();
  let mut dist_lengths = HashMap::<u16, usize>::new();
  let max_literal = num_literals as usize - 1;
  // Generate the code lengths for literas and distances by using an enumeration
  // over the raw lengths to split them. NB: this is a monstrosity.
  for (j, length) in lengths.into_iter().enumerate() {
    let symbol = j as u16;
    let symlen = length as usize;
    if j <= max_literal {
      literal_lengths.insert(symbol, symlen);
    } else {
      let symbol = j - max_literal - 1;
      let symbol = symbol as u16;
      dist_lengths.insert(symbol, symlen);
    }
  }
  let litlen_code = huffcode_from_lengths(&literal_lengths);
  let dist_code = huffcode_from_lengths(&dist_lengths);

  let litlen_tree = match compile_read_tree(litlen_code) {
    Ok(x) => x,
    Err(_) => return Err(DeflateReadError::HuffTreeError),
  };
  let dist_tree = match compile_read_tree(dist_code) {
    Ok(x) => x,
    Err(_) => return Err(DeflateReadError::HuffTreeError),
  };

  Ok((litlen_tree, dist_tree))
}

impl CodeLengthCodepoint {
  fn write_to_bitstream<W: Write>(
    &self,
    bit_sink: &mut BitWriter<W, LittleEndian>,
    codelength_tree: &DeflateWriteTree,
  ) -> std::io::Result<()> {
    match &self {
      Self::Length(x) => bit_sink.write_huffman(codelength_tree, *x as u16),
      Self::ValueRepeat(rep) => {
        assert!(*rep >= 3 && *rep <= 6, "Rep out of range for value repeat");
        bit_sink.write_huffman(codelength_tree, 16);
        bit_sink.write(2, (*rep - 3) as u32)
      }
      Self::ShortZeroRep(rep) => {
        let r = *rep;
        assert!(r >= 3 && r <= 10, "Rep out of range for short zero repeat");
        bit_sink.write_huffman(codelength_tree, 17);
        bit_sink.write(3, r as u32 - 3)
      }
      Self::LongZeroRep(rep) => {
        let r = *rep;
        assert!(r >= 11 && r <= 138, "Rep out of range for long zero rep");
        bit_sink.write_huffman(codelength_tree, 18);
        bit_sink.write(7, r as u32 - 11)
      }
    }
  }

  fn read_from_bitstream<R: Read>(
    bit_src: &mut BitReader<R, LittleEndian>,
    codelength_tree: &[DeflateReadTree],
  ) -> Result<Self, DeflateReadError> {
    let codept = bit_src.read_huffman(codelength_tree)?;
    let res = match codept {
      0..=15 => Self::Length(codept as u8),
      16 => {
        let add: u8 = bit_src.read(2)?;
        Self::ValueRepeat(3 + add)
      }
      17 => {
        let add: u8 = bit_src.read(3)?;
        Self::ShortZeroRep(3 + add)
      }
      18 => {
        let add: u8 = bit_src.read(7)?;
        Self::LongZeroRep(11 + add)
      }
      _ => panic!("Codepoint out of range when reading code length (valid 0-18)"),
    };
    Ok(res)
  }
}

fn runlength_encode<S: Copy + PartialEq + Eq>(data: &[S]) -> Vec<(S, u8)> {
  let mut output = Vec::new();
  if data.is_empty() {
    return output;
  }

  let mut cur = (data[0], 1);
  for i in 1..data.len() {
    if data[i] == cur.0 {
      cur = (cur.0, cur.1 + 1);
    } else {
      output.push(cur);
      cur = (data[i], 1);
    }
  }
  output.push(cur);
  output
}

/// Pack nonzero repeats into the best possible units of 3-6 reps.
fn break_range_nonzero_len(mut rep: u8) -> Vec<u8> {
  assert!(rep >= 3, "Cannot encode repeats of 2 or fewer");
  let mut out = Vec::new();
  while rep >= 9 {
    rep -= 6;
    out.push(6);
  }
  assert!(rep >= 3 && rep < 9, "Invalid range for rep after subloop");
  if rep == 8 {
    out.extend(&[4, 4]);
  } else if rep == 7 {
    out.extend(&[4, 3]);
  } else {
    out.push(rep);
  }
  out
}

fn break_range_zero_len(mut rep: u8) -> Vec<u8> {
  assert!(rep >= 3, "Cannot encode repeats of 2 or fewer");
  let mut out = Vec::new();
  while rep >= 138 {
    out.push(138);
    rep -= 138;
  }
  while rep >= 13 {
    out.push(10);
    rep -= 10;
  }
  assert!(rep >= 3 && rep < 13, "Invalid ragne for rep after subloop");
  if rep == 12 {
    out.extend(&[6, 6]);
  } else if rep == 11 {
    out.extend(&[6, 5]);
  } else {
    out.push(rep);
  }
  out
}

fn code_lengths_to_codepoints(lengths: &[u8]) -> Vec<CodeLengthCodepoint> {
  let mut output = Vec::new();
  let rle = runlength_encode(&lengths[..]);
  for (len, rep) in rle {
    if rep == 1 {
      output.push(CodeLengthCodepoint::Length(len));
    } else if rep == 2 {
      // DEFLATE does not let us encode duplets.
      output.push(CodeLengthCodepoint::Length(len));
      output.push(CodeLengthCodepoint::Length(len));
    } else if rep == 3 && len != 0 {
      output.push(CodeLengthCodepoint::Length(len));
      output.push(CodeLengthCodepoint::Length(len));
      output.push(CodeLengthCodepoint::Length(len));
    } else {
      // Push zero-length repeat codes
      if len == 0 {
        for r in break_range_zero_len(rep) {
          if r <= 10 {
            output.push(CodeLengthCodepoint::ShortZeroRep(r));
          } else {
            output.push(CodeLengthCodepoint::LongZeroRep(r));
          }
        }
      } else {
        // Push value-based repeat codes
        output.push(CodeLengthCodepoint::Length(len));
        for r in break_range_nonzero_len(rep - 1) {
          output.push(CodeLengthCodepoint::ValueRepeat(r));
        }
      }
    }
  }
  output
}

fn codepoints_to_code_lengths(pts: &[CodeLengthCodepoint]) -> Option<Vec<u8>> {
  let mut output = Vec::new();
  let mut lastlength = None;
  for clcp in pts {
    match clcp {
      CodeLengthCodepoint::Length(x) => {
        output.push(*x);
        lastlength = Some(x);
      }
      CodeLengthCodepoint::ValueRepeat(n) => {
        if let Some(l) = lastlength {
          for _ in 0..*n {
            output.push(*l);
          }
        } else {
          return None; // No valid length to repeat
        }
      }
      CodeLengthCodepoint::LongZeroRep(n) | CodeLengthCodepoint::ShortZeroRep(n) => {
        for _ in 0..*n {
          output.push(0);
        }
      }
    }
  }
  Some(output)
}

mod test {
  #[allow(unused_imports)]
  use super::*;

  #[test]
  fn simple_codelength_test() {
    let x = vec![0u8, 0, 0, 0, 0, 0, 1, 2, 3, 4, 4, 4, 4, 4, 4];
    let y = code_lengths_to_codepoints(&x[..]);
    let expected = vec![
      CodeLengthCodepoint::ShortZeroRep(6),
      CodeLengthCodepoint::Length(1),
      CodeLengthCodepoint::Length(2),
      CodeLengthCodepoint::Length(3),
      CodeLengthCodepoint::Length(4),
      CodeLengthCodepoint::ValueRepeat(5),
    ];
    assert_eq!(y, expected);
  }

  #[test]
  fn roundtrip_codelength_1() {
    let lengths = vec![
      0u8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 1, 2, 3, 3, 3, 2,
      2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 2, 2, 1,
    ];

    let encoded = code_lengths_to_codepoints(&lengths[..]);
    let decoded = codepoints_to_code_lengths(&encoded[..]).unwrap();
    assert_eq!(lengths, decoded);
  }

  #[test]
  fn roundtrip_codelength_2() {
    let lengths = vec![
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3,
      2, 1, 0,
    ];

    let encoded = code_lengths_to_codepoints(&lengths[..]);
    let decoded = codepoints_to_code_lengths(&encoded[..]).unwrap();
    assert_eq!(lengths, decoded);
  }

  #[test]
  fn roundtrip_codelength_3() {
    let lengths = vec![
      0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 5, 0, 0, 0, 0, 0, 0,
    ];

    let encoded = code_lengths_to_codepoints(&lengths[..]);
    let decoded = codepoints_to_code_lengths(&encoded[..]).unwrap();
    assert_eq!(lengths, decoded);
  }

  #[test]
  fn roundtrip_codelength_codepts() {
    let mut v = Vec::new();
    for i in 0..=15 {
      v.push(CodeLengthCodepoint::Length(i));
    }
    for i in 3..=6 {
      v.push(CodeLengthCodepoint::ValueRepeat(i));
    }
    for i in 3..=10 {
      v.push(CodeLengthCodepoint::ShortZeroRep(i))
    }
    for i in 11..=138 {
      v.push(CodeLengthCodepoint::LongZeroRep(i))
    }

    let mut buf = [0u8; 16384];

    let readtree = super::super::default_data::default_hufftree::default_read_hufftree();
    let writetree = super::super::default_data::default_hufftree::default_write_hufftree();

    let mut w = BitWriter::new(&mut buf[..]);

    for pt in v.iter() {
      pt.write_to_bitstream(&mut w, &writetree);
    }

    let mut r = BitReader::new(&buf[..]);
    let mut syms = Vec::new();
    for _ in 0..v.len() {
      syms.push(CodeLengthCodepoint::read_from_bitstream(&mut r, &readtree).unwrap());
    }

    assert_eq!(syms, v);
  }
}
