#!/bin/bash

scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
bin_dir="$scriptdir/src"

if [ "$1" = "" ] || [ "$2" = "" ]; then
    echo "Convert a file into an LZ77-protobuf"
    echo "Usage: $0 <inputfile> <output_lzproto>"
    exit 1
fi


if [ ! -f "$bin_dir/gensa" ] || [ ! -f "$bin_dir/count" ]; then
    pushd "$bin_dir"
    make
    popd
fi

input_fpath="$1"
input_fname="$(basename -s .bin $1)"
sa_file="${input_fpath}.sa"
output_fname="$2"

$bin_dir/gensa "$input_fpath" "$sa_file"
$bin_dir/count "$input_fpath"

# Cleanup
rm "$sa_file"
mv "${input_fpath}.lzproto" "$2"
