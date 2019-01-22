#!/bin/bash
#----------------------------------------------------
# Example Slurm job script
# for TACC Stampede2 SKX nodes
#
#   *** Hybrid Job on SKX Normal Queue ***
# 
#       This sample script specifies:
#         10 nodes (capital N)
#         40 total MPI tasks (lower case n); this is 4 tasks/node
#         12 OpenMP threads per MPI task (48 threads per node)
#
# Last revised: 20 Oct 2017
#
# Notes:
#
#   -- Launch this script by executing
#      "sbatch skx.mpi.slurm" on Stampede2 login node.
#
#   -- Use ibrun to launch MPI codes on TACC systems.
#      Do not use mpirun or mpiexec.
#
#   -- In most cases it's best to keep
#      ( MPI ranks per node ) x ( threads per rank )
#      to a number no more than 48 (total cores).
#
#   -- If you're running out of memory, try running
#      fewer tasks and/or threads per node to give each 
#      process access to more memory.
#
#   -- IMPI and MVAPICH2 both do sensible process pinning by default.
#
#----------------------------------------------------

#SBATCH -J 1csp-equil-knl          # Job name
#SBATCH -o 1csp-equil-knl.o%j       # Name of stdout output file
#SBATCH -e 1csp-equil-knl.e%j       # Name of stderr error file
#SBATCH -p normal      # Queue (partition) name
#SBATCH -N 3              # Total # of nodes 
#SBATCH -n 39              # Total # of mpi tasks
#SBATCH -t 10:00:00        # Run time (hh:mm:ss)
#SBATCH --mail-user=kcsong+tacc@utexas.edu
#SBATCH --mail-type=all    # Send email at begin and end of job
#SBATCH -A A-ti3       # Allocation name (req'd if you have more than 1)

# Other commands must follow all #SBATCH directives...

module list
pwd
date

module load intel/16.0.3 impi namd/2017_12_05

ibrun namd2_knl +ppn 8 +pemap 0-51+68 +commap 52-67 forward.namd | tee forward.log

# ---------------------------------------------------
