use crate::deflate::decoder::DeflateReadError;
use crate::deflate::DeflateSym;

/// Expand a backref at the given point in the data.
fn expand_backref(length: u16, distance: u16, data: &mut Vec<u8>) -> Result<(), DeflateReadError> {
  let last_data_index = data.len();
  if distance as usize > last_data_index {
    return Err(DeflateReadError::BackrefPastStart(
      distance,
      last_data_index,
    ));
  }
  let first_i = last_data_index - distance as usize;
  if length > distance {
    let mut n = 0u16;
    loop {
      for j in first_i..last_data_index {
        let target = data[j];
        data.push(target);
        n += 1;
        if n == length {
          return Ok(());
        }
      }
    }
  } else {
    let last_i = first_i + length as usize - 1;
    for j in first_i..=last_i {
      let target = data[j];
      data.push(target);
    }
    Ok(())
  }
}

/// Decode a partially-decoded LZ77 stream. Appends symbols to decoded vector
pub fn decode_lz77(data: &[DeflateSym], decoded: &mut Vec<u8>) -> Result<(), DeflateReadError> {
  for sym in data.iter() {
    match sym {
      DeflateSym::Literal(ch) => decoded.push(*ch),
      DeflateSym::Backreference(length, distance) => {
        expand_backref(*length, *distance, decoded)?;
      }
      DeflateSym::EndOfBlock => break,
    }
  }
  Ok(())
}
