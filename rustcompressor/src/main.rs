use std::env;
use std::fs;
use std::process;

use compressor::huff_tree::HuffEncoder;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        println!("Usage: {} <infilename>", &args[0]);
        process::exit(1);
    }

    let infilename = &args[1];
    let contents = fs::read_to_string(infilename)
        .expect("Something went wrong reading the file")
        .into_bytes();

    let coder = HuffEncoder::new(&contents);
    let encoded = coder.encode(&contents);

    println!(
        "Ratio is {}",
        (encoded.len() as f64 / 8.0) / contents.len() as f64
    );
}
