#!/bin/bash

## This is the driver script for the multisim slurm script. The script does the
## following tasks:
##    - Set up default and required arguments
##    - Make a temporary script with the contents of the skeleton SLURM script
##    - Replace the appropriate strings in the copy with values read in from
##      arguments to this script.
##    - Submit script to SLURM

set -euo pipefail

# Determine TACC host so we can set the queue and default time arguments
FQDN=$(dnsdomainname)
HOSTNAME_EXPR="([a-zA-Z0-9]+)\.tacc.utexas.edu"
if [[ $FQDN =~ $HOSTNAME_EXPR ]]; then
    HOSTNAME=${BASH_REMATCH[1]}
else
    >&2 echo "Could not determine TACC hostname. Aborting."
    exit 1
fi

# Due to quirks in this script, "INVALID" will be treated as an invalid name
# for all options. Hopefully nobody ever needs to run a job named "INVALID"

VERBOSE=false
JOBNAME=INVALID
JOBTIME=24:00:00
ALLOCNAME=Protein-Compression
WORKDIR=INVALID
CONTINUE=false
if [ "$HOSTNAME" = "ls5" ]; then
    QUEUE=gpu; 
elif [ "$HOSTNAME" = "maverick2" ]; then
    QUEUE=gtx; 
else 
    >&2 echo "Not a known host--cannot set queue" 
    exit 1
fi

show_help() {
cat << EOF
Usage: ${0##*/} [-hvc] [-a PROJECT] [-t TIME] [-q QUEUE] [-j JOBNAME] WORKDIR
Prep a 

    -h          display this help and exit
    -v          Run in verbose mode
    -c          continue the job from an aborted run, instead of starting a 
                fresh run.
    -a          Project allocation to use, the name displayed by taccinfo
    -t          Max runtime, in HH:MM:SS format. Defaults to max for the default
                queue
    -q          The queue to submit the job on. Defaults are selected based on
                the host that the script is run on.
    -j          A string that identifies the name of the job. If unspecified, is
                set based on the WORKDIR argument.
    WORKDIR     The directory in which to find the input files for the job,
                including a script called ${WORKDIR}.sh that contains the
                commands to run for the simulation. This is the directory created
                by Maestro when it is asked to "write" the job instead of 
                running it. This argument is mandatory.
EOF
}


OPTIND=1
while getopts hvca:t:q:j: opt; do
    case $opt in
        h)  show_help
            exit 0
            ;;
        v)  VERBOSE=true
            ;;
        c)  CONTINUE=true
            ;;
        a)  ALLOCNAME="$OPTARG"
            ;;
        t)  JOBTIME="$OPTARG"
            ;;
        q)  QUEUE="$OPTARG"
            ;;
        j)  JOBNAME="$OPTARG"
            ;;
        *)  show_help >&2
            exit 1
            ;;
    esac
done
shift "$((OPTIND-1))"   # Discard the options and sentinel

# All remaining text in $@ is a non-option. There should only be one--WORKDIR.
WORKDIR="$@"

# Validate arguments
if [ "$WORKDIR" =  "INVALID" ]; then
    >&2 echo "[ERR]: WORKDIR was not provided. Pass -h to see help"
    exit 1
fi

if [ ! -d "$WORKDIR" ]; then
    >&2 echo "[ERR]: WORKDIR must be a directory."
    exit 1
fi

# At this point, we know that WORKDIR is a valid value. If JOBNAME has not been
# set, use the value of WORKDIR to populate JOBNAME
if [ "$JOBNAME" = "INVALID" ]; then
    JOBNAME="$(basename $WORKDIR)"
fi

# Reset WORKDIR to be an absolute path
WORKDIR="$(realpath "$WORKDIR")"

# If in verbose mode, display all options
if [ $VERBOSE = "true" ]; then
    echo "JOBNAME = $JOBNAME"
    echo "JOBTIME = $JOBTIME"
    echo "ALLOCATION = $ALLOCNAME"
    echo "WORKDIR = $WORKDIR"
    echo "CONTINUE = $CONTINUE"
    echo "QUEUE = $QUEUE"
    echo "HOSTNAME = $HOSTNAME"
fi

# Make a temporary copy
SCRIPTDIR="$(dirname "$(readlink -f "$0")")"
TEMPSCRIPT=$(mktemp)
cp "$SCRIPTDIR/multisim-template.slurm" "$TEMPSCRIPT"

if [ $VERBOSE = "true" ]; then
    echo "Temporary script at $TEMPSCRIPT"
fi

# sed the appropriate symbols
sed -i "s/%JOBNAME%/$JOBNAME/g" $TEMPSCRIPT
sed -i "s/%JOBTIME%/$JOBTIME/g" $TEMPSCRIPT
sed -i "s/%ALLOCNAME%/$ALLOCNAME/g" $TEMPSCRIPT
sed -i "s#%WORKDIR%#$WORKDIR#g" $TEMPSCRIPT
sed -i "s/%CONTINUE%/$CONTINUE/g" $TEMPSCRIPT
sed -i "s/%QUEUE%/$QUEUE/g" $TEMPSCRIPT
sed -i "s/%HOSTNAME%/$HOSTNAME/g" $TEMPSCRIPT

# launch script
sbatch $TEMPSCRIPT