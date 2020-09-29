use std::env;
use std::process;

//use compressor::deflate::DeflateStream;
use compressor::deflate::*;

fn main() {
  let args: Vec<String> = env::args().collect();

  if args.len() != 3 {
    println!("Usage: {} <infilename> <outfilename>", &args[0]);
    println!("\tAttempts to encode file contents, printing the sizes of the input and compressed result");
    process::exit(1);
  }

  let infilename = &args[1];
  let bytes = std::fs::read(infilename).unwrap();

  let stream = DeflateStream::new_from_raw_bytes_deflate(&bytes);
  let z: Vec<u8> = Vec::new();
  let out = stream.write_to_bitstream(z).unwrap();

  std::fs::write(&args[2], out);
}
