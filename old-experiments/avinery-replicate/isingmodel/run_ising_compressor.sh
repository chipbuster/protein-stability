#!/bin/bash

packTypes=(bit byte)
layoutTypes=(random row col spacefill spiral)

source /opt/anaconda/anaconda3/bin/activate

parallel -P 8 python compressstate_ising.py ::: ${layoutTypes[@]} ::: ${packTypes[@]} ::: datafiles/*.pkl

echo "temp\tlayout\tbb\tratio" > aaaa.tsv

rm -f ratios.tsv  # delete existing ratios to avoid double data in file
cat *.tsv > ratios.tmp
rm *.tsv
mv ratios.tmp ratios.tsv
