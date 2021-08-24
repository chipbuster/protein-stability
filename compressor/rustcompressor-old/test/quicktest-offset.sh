#!/bin/bash

tmpd=$(mktemp -d)
for j in $(seq 1 10); do
    tmpf=$(mktemp -p $tmpd)
    echo "Generating random file"
    dd bs=1 count=10M if=/dev/urandom of=$tmpf
    echo "Compressing"
    time RUST_BACKTRACE=1 cargo run --release --bin offset_encode_size $tmpf /dev/null
    if [ $? -ne 0 ]; then
        echo "Encode failed on $tmpf"
        exit 1
    fi
done
