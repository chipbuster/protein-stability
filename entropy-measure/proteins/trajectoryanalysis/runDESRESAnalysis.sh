#!/bin/bash

DATADIR=/home/chipbuster/Spinny/PDBStorage/piana2012
SCRIPTDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
SCRIPT="$SCRIPTDIR/replicatearxiv.py"

find $DATADIR -mindepth 1 -maxdepth 1 -type d -print0 | while IFS= read -r -d $'\0' PROTNAME_RAW; do
    PROTNAME=$(basename $PROTNAME_RAW)
    STAGE1="DESRES-Trajectory_$PROTNAME-protein"
    STAGE2="$PROTNAME-protein"
    FULLPATH="$DATADIR/$PROTNAME/$STAGE1/$STAGE2"
    cd "$FULLPATH"

    echo "Running compression test for $PROTNAME"

    $SCRIPT "$SCRIPTDIR/$PROTNAME-output.txt" $PROTNAME-protein.pdb $PROTNAME-*.dcd
done
