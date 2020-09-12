use std::env;
use std::fs;
use std::process;

use compressor::gzip::reader::*;

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
  } else {
    println!("{}", gzblob.unwrap());
  }
}
