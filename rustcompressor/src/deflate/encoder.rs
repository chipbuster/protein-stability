use super::default_data::default_codepoints::DecodeInfo;
use bitstream_io::{LittleEndian, BitWriter};
use thiserror::Error;
use std::io::Write;

#[derive(Error, Debug)]
pub enum DeflateWriteError {
    #[error("Other IO Error: {0}")]
    IOError(#[from] std::io::Error)
}
/*
fn write_uncompressed_block(data: &[DeflateS])

impl BlockData {

}

impl Block {

}

impl DeflateStream {

}
*/