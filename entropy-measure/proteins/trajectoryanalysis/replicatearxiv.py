#!/usr/bin/env python3

import pickle as pkl,os
from multiprocessing import Pool
from copy import copy
import mdtraj as md
from compressstate_ic import *
from extraction import *

try:
    outName = sys.argv[1]
    topofile = sys.argv[2]
    trajfiles = sys.argv[3:]
except:
    print("Usage:%s <path-to-output> <path-to-topo> <path-to-traj> [additional traj]")
    print("Topo file: describes connectivity of molecule, e.g. PSF, PDB")
    print("Traj file: describes positions of atoms, e.g. DCD, TRJ")
    sys.exit(1)

# Univ prime is the original input data--it should never be edited
traj = md.load(trajfiles,top=topofile)


maxFrame = len(traj)
print("Loaded trajectory with " + str(maxFrame) + " frames")

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

startFrameList = list(range(0,maxFrame-startEvery, startEvery))

cacheName = "./cachedTrajectoryParse.pkl"

if os.path.exists(cacheName):
    allframes = pkl.load(open(cacheName, 'rb'))
else:
    allframes = extraction.convert_IC(univPrime)
    pkl.dump(allframes,open(cacheName,'wb'))


def runCompressorInstance(index):
    startFrame = startFrameList[index]
    c = CompressionData(allframes, numStates, keepEvery, 
                        start = startFrame, stop = startFrame+windowSize)
    ratio = c.get_compression_ratios()
    frameTime = startFrame * usPerFrame
    return (frameTime, ratio)

p = Pool(14)
outputList = p.map(runCompressorInstance, range(len(startFrameList)))

with open(outName,"w") as outf:
    for (time, ratio) in outputList:
        outf.write(str(time) + "\t" + str(ratio) + "\n")