use std::{
  env,
  io::Read,
  io::Seek,
  io::{SeekFrom, Write},
  process,
};

use protobuf::Message;

fn main() -> Result<(), std::io::Error> {
  let args: Vec<String> = env::args().collect();

  if args.len() != 2 {
    println!("Usage: {} <proto>", &args[0]);
    process::exit(1);
  }

  let mut infile = std::fs::File::open(&args[1])
    .unwrap_or_else(|_| panic!("Could not open input file {}", args[1]));

  let mut cis = protobuf::CodedInputStream::new(&mut infile);
  let d = DEFLATEStream::parse_from(&mut cis).unwrap();

  println!("{:?}", d);

  Ok(())
}
