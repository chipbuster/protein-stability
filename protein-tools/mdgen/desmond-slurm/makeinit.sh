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
FORCE_JOB_OVERRIDE=false
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
while getopts fhvca:t:q:j: opt; do
    case $opt in
        f)  FORCE_JOB_OVERRIDE=true
            ;;
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
WORKDIR_NAME="$(basename "$WORKDIR")"

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

if [ "$CONTINUE" = true ]; then
    # Identify the structure of the given directory. Assume it uses the standard structure:
    # $NAME.{0..9} and an additional directory called ${NAME}_base that contains the
    # default files. Find the first empty directory that does not have a COMPLETE flag
    # and continue the computation there
    for i in {0..9}; do
        CONTINUATION_DIR="$WORKDIR/${WORKDIR_NAME}.$i"
        if [ -f "$CONTINUATION_DIR/COMPLETE" ]; then
            continue
        fi
        SIM_DIR="$CONTINUATION_DIR"
        PREV_DIR="$WORKDIR/${WORKDIR_NAME}.$((i-1))"
        break;
    done

    echo "Moving work from $PREV_DIR into $SIM_DIR"
    cp -ar "$PREV_DIR/." "$SIM_DIR"
else
    PREV_DIR="$WORKDIR/${WORKDIR_NAME}_base"
    SIM_DIR="$WORKDIR/${WORKDIR_NAME}.0"

    if [ "$(ls -A $SIM_DIR)" ]; then
      if [ "$FORCE_JOB_OVERRIDE" = false ]; then
      echo "Cowardly refusing to force overwrite of existing MD job."
      echo "Use -f flag to force an overwrite."
      exit 1 
      fi
    fi

    cp -ar "$PREV_DIR/."  "$SIM_DIR"
fi

IN_CFG_FILE="$SIM_DIR/${WORKDIR_NAME}.cfg"
OUT_CFG_FILE="$SIM_DIR/${WORKDIR_NAME}-out.cfg"

# We need to continue the job. To do so, we need to do a few things:
#  - Figure out the ending timestep and the initial timestep and extend to
#    an appropriate level. This is done by parsing the generated cfg files.
#  - Copy all files into a new directory for job extension 
#    as per https://www.schrodinger.com/kb/126
#  - Construct the appropriate command line invocation. We use a Desmond
#    invocation and not a multisim one as we only have to do MD as opposed
#    to standard relaxations--but this is done in the SLURM script.

if [ "$CONTINUE" = true ]; then
    # Search for last_time in output config file
    while read -r line; do
        if [[ $line =~ \ *last_time\ =\ ([0-9\.]+) ]]; then
            LAST_TIME=${BASH_REMATCH[1]}
        fi
    done < "$OUT_CFG_FILE"

    # Search for time in input cfg (this may be suspect)
    while read -r line; do
        if [[ $line =~ \ *time\ =\ ([0-9\.]+) ]]; then
            INIT_TIME=${BASH_REMATCH[1]}
        fi
    done < "$IN_CFG_FILE"
    NEWTIME=$(echo "$LAST_TIME + $INIT_TIME" | bc)

    if [ $VERBOSE = true ]; then
        echo "Continuation: last time in simulation was $LAST_TIME while the "
        echo "increment is $INIT_TIME. New final time is $NEWTIME"
    fi
fi

# sed the appropriate symbols
sed -i "s/%JOBNAME%/$JOBNAME/g" $TEMPSCRIPT
sed -i "s/%JOBTIME%/$JOBTIME/g" $TEMPSCRIPT
sed -i "s/%ALLOCNAME%/$ALLOCNAME/g" $TEMPSCRIPT
sed -i "s#%WORKDIR%#$SIM_DIR#g" $TEMPSCRIPT
sed -i "s/%CONTINUE%/$CONTINUE/g" $TEMPSCRIPT
sed -i "s/%QUEUE%/$QUEUE/g" $TEMPSCRIPT
sed -i "s/%HOSTNAME%/$HOSTNAME/g" $TEMPSCRIPT

if [ $CONTINUE = true ]; then
    sed -i "s/%NEWTIME%/$NEWTIME/g" $TEMPSCRIPT
fi

# launch script
sbatch $TEMPSCRIPT
