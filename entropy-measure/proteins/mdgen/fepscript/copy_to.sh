#!/bin/bash

# Copy this directory's contents to the desired target

FILES=(namd-templates automutate.tcl mutate_protein.sh patchdim.py)
SRCDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

for f in ${FILES[@]}; do
    cp -r "$SRCDIR/$f" "$1"
done
