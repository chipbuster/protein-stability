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
    println!("\tDisplays information about decoded file");
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
  println!("Encoded data is {} bytes", gzblob.clone().into_data().len());

  println!("Attempting to decode data...");
  let data = gzblob.get_data_copy();
  let decoded = {
    let strm = DeflateStream::new_from_deflate_encoded_bits(data.as_slice()).unwrap();
    strm.into_byte_stream().unwrap()
  };
  println!("Decoded data is {} bytes", &decoded.len());
  println!("Decoded bytes: {:02x?}", &decoded);
  println!("Decoded String: {:?}", std::str::from_utf8(&decoded));
}
