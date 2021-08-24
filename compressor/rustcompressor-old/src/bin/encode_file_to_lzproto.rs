use std::env;
use std::io::BufReader;
use std::process;

use std::io::Write;

//use compressor::deflate::DeflateStream;
use compressor::deflate::{
  lz77::encoder::{LZMaximums, LZRules},
  *,
};

use prost::Message;

// Parse the maximum limits on a string
fn parse_maxopt(params: &[String]) -> LZMaximums {
  match params.len() {
    1 => match params[0].as_ref() {
      "default" => LZMaximums::default(),
      "max" => LZMaximums::max(),
      _ => panic!(
        "Got value: \"{}\" that wasn't \"max\" or \"default\" ",
        params[0]
      ),
    },
    2 => {
      let len = params[0]
        .parse::<usize>()
        .expect("Could not parse usize from length");
      let dist = params[1]
        .parse::<usize>()
        .expect("Could not parse usize from dist");
      LZMaximums::new(len, dist)
    }
    x => panic!("Expected 1 or two arguments, but got {}", x),
  }
}

fn main() {
  let args: Vec<String> = env::args().collect();

  if args.len() != 4 && args.len() != 5 {
    println!("Usage: {} <infilename> <outfilename> <maxopts>", &args[0]);
    println!(
      r#"Encodes file contents to an LZ77 stream, dumping the result into <outfilename>
as a protobuf file following the schema in lz77info.proto. <maxopts>
specify the maximum distance and length for backreferences. They can be one of
three values:
  - "max" for the maximum allowed by the underlying Rust datatype (usually u64)
  - "default" for the default values used by RFC 1951, the DEFLATE RFC
  - two unsigned integers, separated by a space, for specific (maxlen, maxdist) values
"#
    );
    process::exit(1);
  }

  pretty_env_logger::init();

  let infilename = &args[1];
  let outfilename = &args[2];
  let maxmatch = parse_maxopt(&args[3..]);
  let bytes = std::fs::read(infilename).unwrap();

  println!("Using maxmatch of {:?}", maxmatch);

  let mut outfile = std::fs::OpenOptions::new()
    .read(false)
    .write(true)
    .create(true)
    .append(false)
    .truncate(true)
    .open(outfilename)
    .unwrap_or_else(|_| panic!("Could not open output file {}", outfilename));

  let rules = LZRules::new(false, maxmatch);

  let stream = LZ77SymStream::from_uncompressed_bytes(&bytes[..], &rules);
  let protomsg = stream.to_cmsg();

  let mut outvec = Vec::with_capacity(protomsg.encoded_len() + 100);
  protomsg.encode(&mut outvec).unwrap();

  println!("Outvec length: {}", outvec.len());

  let testvec = outvec.clone();
  let test = compressor::deflate::lz77::proto::proto::Compressed::decode(&testvec[..]).unwrap();
  assert_eq!(test, protomsg);

  outfile.write_all(&outvec[..]).unwrap();
}
