#!/usr/bin/env python3
import os
import subprocess
import mdtraj as md
from compressstate_ic import *

mydir = os.path.dirname(os.path.realpath(__file__))

try:
    topofile = sys.argv[3]
    trajfile = sys.argv[4:]
    bincount = int(sys.argv[1])
    skipframes = int(sys.argv[2])
except:
    print("Usage:%s <#bins> <#frameskip> <path-to-topo> <path-to-traj> [additional traj]")
    print("Topo file: describes connectivity of molecule, e.g. PSF, PDB")
    print("Traj file: describes positions of atoms, e.g. DCD, TRJ")
    sys.exit(1)

# Our input file is a Desmond CMS--we need to use VMD to convert it to a PDB
# because MDTraj can't read CMS (and PyMol conversion loses unit cell info)
if topofile[-3:] == "cms":
    outfile = topofile[:-3] + "pdb"
    if not os.path.exists(outfile):
        print("Converting CMS to PDB with VMD, please be patient...", end="")
        convertscript = os.path.join(mydir,"convertcms.tcl")

        exitstatus = subprocess.run(["vmd","-dispdev","text","-e",convertscript,
                        "-args",topofile,outfile])
        exitstatus.check_returncode()  #If conversion was unsuccessful, abort prog
        
        print("...Done!")
    else:
        print("PDB already exists, skipping conversion.")

    topofile = outfile  #Change the topology file to point to the new PDB file

traj = md.load(trajfile, top=topofile)
slicedtraj = traj[::skipframes]

print("Trajectory loaded")

c = CompressionData(traj, bincount)
print("Compression ratio is: " + str(c.get_compression_ratios()))