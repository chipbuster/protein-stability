#!/bin/bash

#SBATCH -J %JOBNAME%                         # Job name
#SBATCH -o %JOBNAME%.%j.out                  # Name of stdout output file (%j expands to jobId)
#SBATCH -e %JOBNAME%.%j.err
#SBATCH -p %QUEUE%                                  # Queue name
#SBATCH -N 1                                    # Total number of nodes requested
#SBATCH -n 1                                    # Total number of mpi tasks requested
#SBATCH -t %JOBTIME%                             # Run time (hh:mm:ss) - 1.5 hours
#SBATCH -A %ALLOCNAME%

set -euo pipefail

# Variables for replacement:
#     %JOBNAME%         Name of job
#     %QUEUE%           Queue to run job on (varies by computer)
#     %JOBTIME%         Max time of job
#     %ALLOCNAME%       Allocation name
#     %WORKDIR%         Directory of the input/output files and calculations
#     %CONTINUE%        Whether to continue a previously aborted run or not
#     %HOSTNAME%        Which TACC host this is being launched from

## General set up: This script is meant to be used as a skeleton script from
## another script, which replaces the text names in the section at the top of
## the file with their actual values. The steps that need to be taken within
## this script are:
##     - Detect and activate the appropriate SCHRODINGER environment
##     - Read and modify the appropriate job, adding continuation flags and
##       flags that cause the SLURM system to behave appropriately.
##       Maestro outputs a script which is designed to be run from the command
##       line in a fully interactive terminal.
##     - Execute the actual job that we want to run (an MDSim)

# We need to know the hostname that the script is being run on.
# This is done because Desmond uses different scratchdirs on different systems,
# and so is installed in different locations.
# Since dynamic determination of the hostname is...difficult from the gpu nodes,
# we instead have the calling script pass it in.
HOSTNAME=%HOSTNAME%

if [ "$HOSTNAME" = "ls5" ]; then
    source ~/scripts/desmond/varset/ls5_activate_desmond.sh
elif [ "$HOSTNAME" = "maverick2" ]; then
    source ~/scripts/desmond/varset/maverick_activate_desmond.sh
else
    >&2 echo "TACC hostname does not match known hosts. Please check SLURM script."
fi

# The Desmond job name is the name of the directory containing the files
if [ ! -d %WORKDIR% ]; then
    >&2 echo "The provided workdir (%WORKDIR%) does not appear to be a directory"
    exit 1
fi

DESMOND_JOBNAME="$(basename %WORKDIR%)"
SCRIPTFILE="%WORKDIR%/${DESMOND_JOBNAME}.sh"
CMDSTRING="$(< "$SCRIPTFILE")"

# To do: Maverick multigpu?

# The base command is now inside CMDSTRING. We need to add the appropriate
# modifier flags based on what the request is and execute it
cd "%WORKDIR%"
if [ "%CONTINUE%" = "true" ]; then
    exit 1 # Deal with this later
else
    #Without the -WAIT flag, multisim will exit, killing the SLURM session and,
    #by extension, the Desmond jobs running in background.
    CMDSTRING="$CMDSTRING -WAIT"
    eval $CMDSTRING
fi