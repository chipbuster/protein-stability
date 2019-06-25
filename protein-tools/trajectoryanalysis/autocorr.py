#!/usr/bin/env python3

## Calculate the sliding-window autocorrelation over a trace

import sys
import os
import subprocess
import numpy as np
import mdtraj as md
from numba import jit

import matplotlib.pyplot as plt

import extraction

topofile = sys.argv[1]
trajfile = sys.argv[2]

# Our input file is a Desmond CMS--we need to use VMD to convert it to a PDB
# because MDTraj can't read CMS (and PyMol conversion loses unit cell info)
if topofile[-3:] == "cms":
    outfile = os.path.join("/tmp",os.path.basename(topofile[:-3] + "pdb"))
    print("Converting CMS to PDB with VMD, please be patient...", end="")
    mydir = os.path.dirname(os.path.abspath(__file__))
    convertscript = os.path.join(mydir,"convertcms.tcl")

    exitstatus = subprocess.run(["vmd","-dispdev","text","-e",convertscript,
                    "-args",topofile,outfile])
    exitstatus.check_returncode()  #If conversion was unsuccessful, abort prog
    
    print("...Done!")
    topofile = outfile  #Change the topology file to point to the new PDB file

print("\nLoading trajectory...",end="")
traj = md.load(trajfile, top=topofile).remove_solvent()
print("...Done!")

iangletrace = extraction.convert_IC(traj)

maxlag = 3000
autocorr = np.empty(maxlag)

# Compute autocorrelation via https://en.wikipedia.org/wiki/Autocorrelation#Auto-correlation_of_stochastic_processes
# R_{xx}(\tau) = \E [<X_{t+\tau}, X_t>]. Since the X_i are all real, this 
# corresponds to a simple inner product
@jit
def compute_autocorr(iatrace, lag):
    print("computing " + str(lag))
    (num_frames,num_angles) = np.shape(iatrace)
    num_samples = num_frames - lag
    autocorr = 0.0
    for i in range(num_samples):
        autocorr += np.dot(iatrace[i,:], iatrace[i + lag,:])
    autocorr /= num_samples
    return autocorr

for tau in range(maxlag):
    autocorr[tau] = compute_autocorr(iangletrace, tau)

tau = np.arange(0,3000,1)

for (t, corr) in enumerate(autocorr):
    print("tau = " + str(t) + ", corr = " + str(corr))

# Check optimal window size by compression result convergence



plt.plot(tau,autocorr)
plt.show()
