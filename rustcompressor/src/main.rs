use std::env;
use std::fs;
use std::process;

use compressor::gzip::GzipData;
use compressor::deflate::DeflateStream;

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
  print!("{}",gzblob);
  let data = gzblob.get_data_copy();

  println!("Attempting to unpack data...");
  let stream = DeflateStream::new_from_source(&data[..]);
  if stream.is_err(){
    println!("{}", stream.unwrap_err());
    process::exit(1);
  } else {
    println!("{:?}", stream.unwrap());
  }
}
