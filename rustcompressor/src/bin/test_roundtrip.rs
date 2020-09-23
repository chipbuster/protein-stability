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
    println!("\tAttempts to roundtrip file contents");
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
  println!("{}", gzblob);
  println!("{}", gzblob.clone().into_data().len());

  println!("Decoding Compressed data...");
  let data = gzblob.get_data_copy();
  let decoded = {
    let strm = DeflateStream::new_from_source(data.as_slice()).unwrap();
    strm.into_byte_stream().unwrap()
  };

  println!("Repacking Data without recomputing LZ77");
  let stream = DeflateStream::new_from_source(data.as_slice()).unwrap();
  let mut z: Vec<u8> = Vec::new();
  let out = stream.write_to_bitstream(z).unwrap();
  let newbyte = DeflateStream::new_from_source(data.as_slice()).unwrap().into_byte_stream().unwrap();
  let newstr = std::str::from_utf8(&newbyte[..]);

  assert_eq!(newbyte, decoded);
  println!("Outputs are equal without re-computing LZ77");

  println!("=================");
  println!("Repacking Data by recomputing LZ77");
  let stream = DeflateStream::new_from_raw_bytes(&decoded);
  let mut z: Vec<u8> = Vec::new();
  let out = stream.write_to_bitstream(z).unwrap();
  let newdecoded = DeflateStream::new_from_source(data.as_slice()).unwrap().into_byte_stream().unwrap();

  assert_eq!(newdecoded, decoded);
  println!("Outputs are equal after re-computing LZ77");
}
