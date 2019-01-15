#!/bin/bash

packTypes=(bit byte)
layoutTypes=(random row col spacefill spiral)

source /opt/anaconda/anaconda3/bin/activate

parallel -P 8 python compressstate_ising.py ::: ${layoutTypes[@]} ::: ${packTypes[@]} ::: datafiles/*.pkl
