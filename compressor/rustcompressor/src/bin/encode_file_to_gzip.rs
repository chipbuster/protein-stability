use std::env;
use std::process;

//use compressor::deflate::DeflateStream;
use compressor::{deflate::*, gzip::GzipData};

fn main() {
  let args: Vec<String> = env::args().collect();

  if args.len() < 2 || args.len() > 4 {
    println!(
      "Usage: {} <infilename> <outfilename> method] [gzip]",
      &args[0]
    );
    println!(
      r#"    Attempts to encode file contents, printing the sizes of the input and compressed result
    [method] determines which deflate variant to use: "offset" or "deflate"   (default: deflate)
    [gzip]   determines whether the output should be wrapped in a GZIP header (default: true)"#
    );
    process::exit(1);
  }

  let infilename = &args[1];
  let outfilename = &args[2];
  let method = args.get(3).map(|x| &x[..]).unwrap_or("deflate");
  let gzip = args.get(4).map(|x| &x[..]).unwrap_or("true") == "true";

  let bytes = std::fs::read(infilename).unwrap();

  let stream = match method {
    "offset" => DeflateStream::new_from_raw_bytes_offset(&bytes),
    "deflate" => DeflateStream::new_from_raw_bytes_deflate(&bytes),
    _ => panic!("Unknown stream type"),
  };

  let z: Vec<u8> = Vec::new();
  let out = stream.write_to_bitstream(z).unwrap();
  let out_nbytes = out.len();

  if gzip {
    let mut gzd = GzipData::new();
    gzd.set_uncompressed_data(bytes);
    gzd.set_compressed_data(out);
    let buf = Vec::new();
    let gzipout = gzd.write_to(buf).unwrap();
    std::fs::write(outfilename, gzipout).unwrap();
  } else {
    std::fs::write(outfilename, out).unwrap();
  }

  println!("File size is {} bytes", out_nbytes);
}
