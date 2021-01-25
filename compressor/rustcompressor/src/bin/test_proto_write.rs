use std::{
  env,
  io::Read,
  io::Seek,
  io::{SeekFrom, Write},
  process,
};

use compressor::deflate::*;
use compressor::gzip::*;
use prost::Message;

fn main() -> Result<(), std::io::Error> {
  let args: Vec<String> = env::args().collect();

  if args.len() != 3 {
    println!("Usage: {} <compressed-in> <json-out>", &args[0]);
    println!("\tDumps a JSON representation of a compressed file to disk for further examination");
    println!("\t<compressed-in> can be either a GZIP file or raw DEFLATE data");
    process::exit(1);
  }

  let mut infile = std::fs::File::open(&args[1])
    .unwrap_or_else(|_| panic!("Could not open input file {}", args[1]));
  let mut outfile = std::fs::OpenOptions::new()
    .read(false)
    .write(true)
    .create(true)
    .append(false)
    .truncate(true)
    .open(&args[2])
    .unwrap_or_else(|_| panic!("Could not open output file {}", args[2]));

  // Check the file to see how we should treat it: is it GZIP or raw DEFLATE?
  let mut magic_buf = [0u8; 2];
  infile.read_exact(&mut magic_buf[..]).unwrap();
  infile.seek(SeekFrom::Start(0)).unwrap();

  let dfs_proto = if magic_buf != [0x1fu8, 0x8b] {
    println!("File is not gzip data--assuming it's raw DEFLATE-encoded");
    let mut data = Vec::new();
    infile.read_to_end(&mut data).unwrap();
    DeflateStream::new_from_deflate_encoded_bits(&data[..])
      .unwrap()
      .into_proto()
  } else {
    println!("File is a GZIP file.");
    let gzip_data = GzipData::new_from_gzip_data(&mut infile).unwrap();
    let mut header_str = String::new();
    gzip_data.fmt_header(&mut header_str).unwrap();
    // println!("{}", header_str);

    let (crc32, isz) = gzip_data.get_checksums();

    let dfs = DeflateStream::new_from_deflate_encoded_bits(&gzip_data.get_data_copy()[..]).unwrap();

    // Check that the data here actually matches the checksums provided
    dfs.clone().into_byte_stream_checked(crc32, isz).unwrap();

    dfs.into_proto()
  };

  println!("Encoding a message length of {}", dfs_proto.encoded_len());

  let mut outvec = Vec::with_capacity(dfs_proto.encoded_len() + 10);
  dfs_proto.encode(&mut outvec).unwrap();

  println!("Outvec length: {}", outvec.len());

  let testvec = outvec.clone();

  let test = proto::proto::DeflateStream::decode(&testvec[..]).unwrap();
  assert_eq!(test, dfs_proto);

  outfile.write(&outvec[..])?;

  Ok(())
}
