use std::env;
use std::process;

//use compressor::deflate::DeflateStream;
use compressor::deflate::*;

fn main() {
  let args: Vec<String> = env::args().collect();

  if args.len() != 4 {
    println!("Usage: {} <infilename> <outfilename> <method>", &args[0]);
    println!(
      r#"    Attempts to encode file contents, printing the sizes of the input and compressed result
    <method> can be "offset" or "deflate""#
    );
    process::exit(1);
  }

  let method = &args[3][..];

  let infilename = &args[1];
  let bytes = std::fs::read(infilename).unwrap();

  let stream = match method {
    "offset" => DeflateStream::new_from_raw_bytes_offset(&bytes),
    "deflate" => DeflateStream::new_from_raw_bytes_deflate(&bytes),
    _ => panic!("Unknown stream type"),
  };

  let z: Vec<u8> = Vec::new();
  let out = stream.write_to_bitstream(z).unwrap();
  let out_nbytes = out.len();

  std::fs::write(&args[2], out).unwrap();
  println!("File size is {} bytes", out_nbytes);
}
