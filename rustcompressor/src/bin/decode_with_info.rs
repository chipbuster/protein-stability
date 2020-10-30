use std::{env, io::Read, io::Seek, io::SeekFrom, process};

use compressor::deflate::*;
use compressor::gzip::*;

fn main() {
  let args: Vec<String> = env::args().collect();

  if args.len() != 2 {
    println!("Usage: {} <infilename>", &args[0]);
    println!("\tDisplays file contents with additional interpretation.");
    process::exit(1);
  }

  // Try to open this as a GZIP file and print if it worked. If so, print the
  // GZIP headers.
  let mut infile = std::fs::File::open(&args[1]).unwrap();
  let mut magic_buf = [0u8; 2];
  infile.read(&mut magic_buf[..]).unwrap();
  infile.seek(SeekFrom::Start(0));
  let data = if magic_buf == [0x1fu8, 0x8b] {
    println!("File is a GZIP file.");
    let gzip_data = GzipData::new_from_gzip_data(&mut infile).unwrap();
    let mut header_str = String::new();
    gzip_data.fmt_header(&mut header_str).unwrap();
    println!("{}", header_str);
    gzip_data.get_data_copy()
  } else {
    println!("File is not gzip data--assuming it's DEFLATE-encoded");
    let mut data = Vec::new();
    infile.read_to_end(&mut data).unwrap();
    data
  };

  // Print trees used for decoding.

  // Get symbols and encodings or symbols and sizes

  // Print things all pretty-like
}

