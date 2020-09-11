/*! A straight implementation of the DEFLATE algorithm as specified in RFC
1951 (https://tools.ietf.org/html/rfc1951). Includes the reference
implementation of the compressor suggested at the end of the RFC. */

mod datastructs;
pub mod decoder;
pub mod encoder;
