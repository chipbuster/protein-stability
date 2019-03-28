#!/usr/bin/env python3
import os
import tempfile
import subprocess
import mdtraj as md
from compressstate_ic import *

mydir = os.path.dirname(os.path.realpath(__file__))

skipframe_calc = False

try:
    topofile = sys.argv[3]
    trajfile = sys.argv[4:]
    bincount = int(sys.argv[1])
    if sys.argv[2][0] == "s":
        skiptime = float(sys.argv[2][1:])
        skipframe_calc = True
    else:
        skipframes = int(sys.argv[2])
except:
    print("Usage:%s <#bins> <#frameskip> <path-to-topo> <path-to-traj> [additional traj]")
    print("Topo file: describes connectivity of molecule, e.g. PSF, PDB")
    print("Traj file: describes positions of atoms, e.g. DCD, TRJ")
    print("Frameskip can be an integer, to describe a number of frames, or a " +
          "floating point number preceeded by 't' to indicate a minimum gap in " +
          "nanoseconds. E.g. '10' is 10 frames, while 's0.01' is at least a " +
          "0.01ns gap between frames")
    sys.exit(1)

# Our input file is a Desmond CMS--we need to use VMD to convert it to a PDB
# because MDTraj can't read CMS (and PyMol conversion loses unit cell info)
if topofile[-3:] == "cms":
    outfile = os.path.join("/tmp",os.path.basename(topofile[:-3] + "pdb"))
    print("Converting CMS to PDB with VMD, please be patient...", end="")
    convertscript = os.path.join(mydir,"convertcms.tcl")

    exitstatus = subprocess.run(["vmd","-dispdev","text","-e",convertscript,
                    "-args",topofile,outfile])
    exitstatus.check_returncode()  #If conversion was unsuccessful, abort prog
    
    print("...Done!")
    topofile = outfile  #Change the topology file to point to the new PDB file

print("\nLoading trajectory...",end="")
traj = md.load(trajfile, top=topofile).remove_solvent()
print("...Done!")

# Calculate frameskips from skiptimes only if user provided a time instead of a skip
if skipframe_calc:
    tsps = traj.timestep    # In picoseconds
    ts = tsps/1000.0
    if ts > skiptime:
        skipframes = 1
    else:
        skipframes = int(skiptime / ts + 1) # Round up

    print("At timestep = " + str(tsps) + " ps, we are skipping " + str(skipframes)
         + " frames")

slicedtraj = traj[::skipframes]

print("Trajectory loaded, timestep is " + str(traj.timestep) + " ps")

c = CompressionData(slicedtraj, bincount)
print("Compression ratio is: " + str(c.get_compression_ratios()))

with open('ratio.txt','w') as outf:
    outf.write(str(c.get_compression_ratios()))