use super::super::*;
use std::vec::Vec;
use bitstream_io::huffman::compile_read_tree;

/// Takes a slice of 0-1 values and performs an "add" on it. Returns None if
/// a non 0/1 input is encountered
pub fn increment_bit_slice(mut arg: Vec<u8>) -> Option<Vec<u8>> {
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

pub fn default_read_hufftree() -> Box<[DeflateReadTree]> {
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

  compile_read_tree::<LittleEndian, u16>(huff_values).unwrap()
}


mod tests {
  #[allow(unused_imports)]
  use super::*;

  #[test]
  fn inc_test_1() {
    let z = vec![0u8, 0u8, 1u8, 1u8];
    assert_eq!(increment_bit_slice(z).unwrap(), vec![0u8, 1u8, 0u8, 0u8]);
  }

  #[test]
  fn inc_test_2() {
    let z = vec![1u8, 1u8, 1u8, 1u8];
    assert_eq!(increment_bit_slice(z).unwrap(), vec![0u8, 0u8, 0u8, 0u8]);
  }
}
