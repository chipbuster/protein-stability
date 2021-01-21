#!/bin/bash

# Generate plots in bash because Gadfly leaks memory like a sieve

Ns="10 20 30"
fs="$(seq 0.1 0.1 0.9)"

parallel -P 2 julia genplot.jl "~/tmp/gzip-data/jsons/pinreflect" {1} {2} "~/tmp/gzip-data/plots/lzstats_{1}_{2}.svg" ::: $Ns ::: $fs