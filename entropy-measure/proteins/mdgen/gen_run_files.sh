#!/usr/bin/env bash

set -euo pipefail

# Change this to change parameter location
PARAMDIR="/work/02711/chipbus/stampede2/data/namd-auxfiles/c36_params"

if [ -z  "${1-}" ]; then
    echo "Usage: $0 <pdbid>"
    exit 1
fi

PDBID="$1"

# Set up mutations
vmd -dispdev text -e ./proteinsetup.tcl -args "$PDBID"

# Patch Dimensions
python2 ./patchdim.py

# Patch others
for file in "*.namd"; do
    sed -i "s#MUTNAME#$PDBID-$MUTID#" $file
    sed -i "s#PARAMETER_DIR#$PARAMDIR#" $file
done

mv *.namd prepared
cp namd-templates/*.slurm prepared
