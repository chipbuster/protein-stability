fn main() -> std::io::Result<()> {
  prost_build::compile_protos(
    &["src/deflate/proto/deflateinfo.proto"],
    &["src/deflate/proto"],
  )?;
  Ok(())
}
