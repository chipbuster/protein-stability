fn main() -> std::io::Result<()> {
  prost_build::compile_protos(
    &["src/deflate/proto/deflateinfo.proto"],
    &["src/deflate/proto"],
  )?;

  prost_build::compile_protos(
    &["src/deflate/lz77/proto/lz77info.proto"],
    &["src/deflate/lz77/proto"],
  )?;
  Ok(())
}
