use compressor::deflate::*;
use prost::Message;

use std::{env, io::Read, process};

fn main() -> Result<(), std::io::Error> {
  let args: Vec<String> = env::args().collect();

  if args.len() != 2 {
    println!("Usage: {} <proto>", &args[0]);
    process::exit(1);
  }

  let mut infile = std::fs::File::open(&args[1])
    .unwrap_or_else(|_| panic!("Could not open input file {}", args[1]));

  let mut buf = Vec::new();
  infile.read_to_end(&mut buf)?;

  let d = proto::proto::DeflateStream::decode(&buf[..]).unwrap();

  let errs = d.validate();

  if errs.is_empty() {
    println!("No errors found for {}", &args[1])
  } else {
    println!("{}", errs);
  }

  Ok(())
}
