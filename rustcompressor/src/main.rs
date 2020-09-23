use std::env;
use std::fs;
use std::process;

//use compressor::deflate::DeflateStream;
use compressor::deflate::*;
use compressor::gzip::GzipData;

fn main() {
  let args: Vec<String> = env::args().collect();

  if args.len() != 2 {
    println!("Usage: {} <infilename>", &args[0]);
    process::exit(1);
  }

  let infilename = &args[1];
  let f = fs::File::open(infilename).unwrap();
  let gzblob = GzipData::new_from_gzip_data(f);
  if gzblob.is_err() {
    println!("{}", gzblob.unwrap_err());
    process::exit(1);
  }

  let gzblob = gzblob.unwrap();
  print!("{}", gzblob);
  let data = gzblob.get_data_copy();

  println!("Attempting to unpack data...");
  let data = gzblob.into_decoded();
  if data.is_err() {
    println!("{}", data.unwrap_err());
    process::exit(1);
  }
  // println!("{:02x?}", data.as_ref().unwrap());
  println!("{:?}", std::str::from_utf8(data.as_ref().unwrap()));

  // Attempt to round-trip data
  let compressed = CompressedBlock::bytes_to_lz77(data.as_ref().unwrap());
  let stream = DeflateStream {
    blocks: vec![Block {
      bfinal: true,
      data: BlockData::Dyn(compressed),
    }],
  };
  let mut z: Vec<u8> = Vec::new();
  let out = stream.write_to_bitstream(z).unwrap();

  println!("{:x?}", out);
}
