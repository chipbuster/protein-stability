use std::collections::HashSet;
use std::env;

//use compressor::deflate::DeflateStream;
use compressor::deflate::{
  lz77::encoder::{LZMaximums, LZRules},
  *,
};
use std::convert::TryInto;

#[derive(Debug, PartialEq, Eq)]
enum Mode {
  /* Use the encoding size of a naively encoded file as the entropy estimate:
  Log (distance to start) + Log (backref len) + log(log(backref len)) for backrefs
  log(num_unique_symbols) for literals */
  EncSize,
  /* Use a suggestion from one of Ziv's papers: simply estimate the entropy as
  Log(sequence lenght) / longest backreference length
  since this should approach H(X) in the infinite limit */
  LogN,
}

#[derive(Debug)]
struct Opts {
  data: Vec<u8>,
  mode: Mode,
  num_syms: u64,
}

fn print_usage(prog_name: &str) {
  println!("Usage: {} <infilename> <opt> [numsyms]", prog_name);
  println!(
    r#"Estimates the entropy of a binary file using unlimited-memory LZ77
algorithms. Writes the resulting (per-symbol) estimate to stdout.
Can run in one of several modes, determined by <opt>:
    - "encsize": Uses a naive encoding of length/distance/literal to estimate
      the entropy via the compression ratio. Uses the number of distinct symbols
      in the input to calculate the ratio.
    - "logn": Uses an idea from Ziv's papers to use the asymptotic ratio of
      the backreference distance to the window size.
"#
  );
}

fn u64_to_f64(x: u64) -> Option<f64> {
  let result = x as f64;
  if result as u64 != x {
    return None;
  }
  Some(result)
}
fn try_parse_args(args: &[String]) -> Option<Opts> {
  let infilename = &args[1];
  let bytes = std::fs::read(infilename).unwrap();

  let mode = match args[2].as_str() {
    "encsize" => Mode::EncSize,
    "logn" => Mode::LogN,
    x => panic!("Invalid encoding mode {}", x),
  };

  let num_syms = match args.get(3) {
    Some(x) => x.parse().unwrap(),
    None => bytes.len() as u64,
  };

  assert!(num_syms > 0, "Have zero symbols in input data");

  Some(Opts {
    data: bytes,
    mode,
    num_syms,
  })
}

fn main() {
  let args: Vec<String> = env::args().collect();

  if args.len() < 3 || args.len() > 4 {
    print_usage(&args[0]);
    std::process::exit(1);
  }

  let opts = try_parse_args(&args).expect("Invalid Arguments");

  pretty_env_logger::init();

  let maxmatch = LZMaximums::max();
  log::info!("Using maxmatch of {:?}", maxmatch);
  let rules = LZRules::new(maxmatch);
  let stream = LZ77SymStream::from_uncompressed_bytes(&opts.data[..], &rules);

  let mut unique_syms = HashSet::new();
  for b in &opts.data[..] {
    unique_syms.insert(*b);
  }
  let num_unique_syms = unique_syms.len();
  log::info!("Number of unique symbols observed: {}", num_unique_syms);

  // All setup complete: do the actual methodology
  match opts.mode {
    Mode::LogN => {
      let longest_backref = stream
        .symbols
        .iter()
        .map(|x| match x {
          LZSym::Backreference(len, _) => *len,
          _ => 0,
        })
        .max()
        .unwrap();
      let logn = u64_to_f64(opts.num_syms).unwrap().log2();
      let l_n = u64_to_f64(longest_backref).unwrap();
      println!("{}", logn / l_n)
    }
    Mode::EncSize => {
      let mut num_bits = 0f64;
      let mut num_syms_so_far = 0u64;
      for sym in stream.symbols {
        num_syms_so_far += 1;
        num_bits += match sym {
          LZSym::Literal(_) => u64_to_f64(num_unique_syms.try_into().unwrap())
            .unwrap()
            .log2(),
          LZSym::Backreference(len, _) => {
            let dist_bits = u64_to_f64(num_syms_so_far).unwrap().log2();
            let lenf64 = u64_to_f64(len).unwrap();
            let len_bits = lenf64.log2() + lenf64.log2().log2();
            dist_bits + len_bits
          }
          _ => 0.0,
        };
      }
      println!("{}", num_bits / u64_to_f64(num_syms_so_far).unwrap());
    }
  }
}
