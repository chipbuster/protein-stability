use std::env;
use std::process;

use compressor::deflate::*;

fn main() {
  let args: Vec<String> = env::args().collect();

  if args.len() != 2 {
    println!("Usage: {} <infilename>", &args[0]);
    println!("\tAttempts to encode file contents");
    process::exit(1);
  }

  let infilename = &args[1];
  let bytes = std::fs::read(infilename).unwrap();

  let stream = DeflateStream::new_from_raw_bytes(&bytes);
  let z: Vec<u8> = Vec::new();
  let out = stream.write_to_bitstream(z).unwrap();

  let ratio = bytes.len() as f64 / out.len() as f64;
  println!("Raw file is {}x larger than compressed", ratio);
  // println!("{:x?}", &out);
}
