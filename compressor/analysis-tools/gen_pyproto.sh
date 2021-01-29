#!/bin/bash

out_dir="$(pwd)"
src_dir="../rustcompressor/src/deflate/proto/"

mkdir -p "$out_dir"
protoc -I=$src_dir --python_out=$out_dir $src_dir/deflateinfo.proto