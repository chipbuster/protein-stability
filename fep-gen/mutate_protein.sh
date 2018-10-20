#!/usr/bin/env bash

set -euo pipefail

# Need following arguments: PDB ID, Mutation ID String, target residue number,
#                           target residue mutation


# Change this to change parameter location
PARAMDIR="/opt/proteins/ffparams/c36_18"

if [ -z  "${4-}" ]; then
    echo "Usage: $0 <pdbid> <mutid string> <residue number> <mutation 3let code>"
    exit 1
fi

PDBID="$1"
MUTID="$2"
RESNM="$3"
MUTCD="$4"

# Set up mutations
vmd -dispdev text -e ./automutate.tcl -args "$PDBID" "$MUTID" "$RESNM" "$MUTCD"

# Patch Dimensions
python2 ./patchdim.py

# Patch others
for file in "*.namd"; do
    sed -i "s#MUTNAME#$PDBID-$MUTID#" $file
    sed -i "s#PARAMETER_DIR#$PARAMDIR#" $file
done

mv *.namd prepared
