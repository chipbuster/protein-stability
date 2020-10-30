/*! This module contains the functions and methods needed to implement
display debugging of a compressed GZIP input. */

use super::DeflateSym;
use super::codepoints::*;
use std::collections::HashMap;
use bit_vec::BitVec;

/** A structure to help the user understand how bits are being decoded from the
    given file */
struct DecodeVizDictionary {
    code_lengths: HashMap<u16, u8>,
    code: HashMap<u16, Vec<u8>>
}

struct DecodeVizSymbol {
    sym: DeflateSym,
    nbits: u8,
    bitrepr: BitVec
}

struct DecodeVizData {
    dict: DecodeVizDictionary,
    syms: Vec<DecodeVizSymbol>
}