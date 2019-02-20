#!/usr/bin/env python3

from MDAnalysis import Universe
from itertools import islice
from compressstate_ic import *


try:
    topofile = sys.argv[1]
    trajfile = sys.argv[2:]
except:
    print("Usage:%s <#bins> <#frameskip> <path-to-topo> <path-to-traj> [additional traj]")
    print("Topo file: describes connectivity of molecule, e.g. PSF, PDB")
    print("Traj file: describes positions of atoms, e.g. DCD, TRJ")
    sys.exit(1)

univ = MDAnalysis.Universe(topofile, trajfile)
#univ.trajectory = univ.trajectory[0:2000:100]

c = CompressionData(univ, 360, 100)