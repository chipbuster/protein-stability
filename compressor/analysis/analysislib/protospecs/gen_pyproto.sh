#!/bin/bash

out_dir="$(pwd)"
src_dir="$(pwd)"

protoc -I=$src_dir --python_out=$out_dir $src_dir/deflateinfo.proto
protoc -I=$src_dir --python_out=$out_dir $src_dir/lz77info.proto