#!/usr/bin/env python3

from MDAnalysis import Universe
from copy import copy
from compressstate_ic import *

try:
    topofile = sys.argv[1]
    trajfile = sys.argv[2:]
except:
    print("Usage:%s <path-to-topo> <path-to-traj> [additional traj]")
    print("Topo file: describes connectivity of molecule, e.g. PSF, PDB")
    print("Traj file: describes positions of atoms, e.g. DCD, TRJ")
    sys.exit(1)

# Univ prime is the original input data--it should never be edited
univPrime = MDAnalysis.Universe(topofile, trajfile)
maxFrame = len(univPrime.trajectory)

# From paper: 
#   - need to define sliding windows of 2000 frames (0.4 us)
#   - Start a window every 200 frames
#   - Use Ns = 11 or 24 (for lowest entropy and highest discrepancy, resp.)
#   - Keep every 100 frames (correlation length of 20-30)
# From README: DCDs are sampled every 0.2ns (agrees with paper)

# Parameters to match the above
startEvery = 200
keepEvery = 100
windowSize = 2000
numStates = 11
usPerFrame = 0.2 * 1e-3 #0.2 nanoseconds and 1/1000 us per ns

with open("arxiv-output.txt",'w') as outfile:
    for startFrame in range(0,maxFrame-startEvery, startEvery):

        univ = copy(univPrime)
        univ.trajectory = univ.trajectory[startFrame:startFrame+windowSize]

        c = CompressionData(univ, numStates, keepEvery)
        ratio = c.get_compression_ratios()
        frameTime = startFrame * usPerFrame

        outfile.write(str(frameTime) + "\t" + str(ratio))