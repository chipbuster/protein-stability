#!/usr/bin/env python3
import os
import tempfile
import subprocess
import mdtraj as md
from compressstate_ic import *

mydir = os.path.dirname(os.path.realpath(__file__))

def normalize_topofile(topofile):
    """ Normalize a topology file into PDB format.

    Use a VMD TCL script to normalize a topology file into a standard format.
    Take in the path to the file to be normalize. Return the path to the new 
    normalized file, creating + normalizing it as a side-effect."""

    outfile = os.path.join("/tmp",os.path.basename(topofile[:-3] + "pdb"))
    #print("Converting CMS to PDB with VMD, please be patient...", end="")
    convertscript = os.path.join(mydir,"convertcms.tcl")

    exitstatus = subprocess.run(["vmd","-dispdev","text","-e",convertscript,
                    "-args",topofile,outfile])
    exitstatus.check_returncode()  #If conversion was unsuccessful, abort prog
    return outfile

def gen_compressor_from_mdtraj(traj, bincount, skipframes_input, verbose):
    """ Create a compressor using an mdtraj and parameters. """
    # Deal with skipframe input
    skipframes_calc = False
    if type(skipframes_input) == int:
        skipframes = skipframes_input
    elif skipframes_input[0] == "s":
        skiptime = float(skipframes_input[1:])
        skipframe_calc = True
    else:
        skipframes = int(skipframes_input)

    if skipframe_calc:
        tsps = traj.timestep    # In picoseconds
        ts = tsps/1000.0
        if ts > skiptime:
            skipframes = 1
        else:
            skipframes = int(skiptime / ts + 1) # Round up

        # Calculate frameskips from skiptimes only if user provided a time instead of a skip
        print("At timestep = " + str(tsps) + " ps, we are skipping " + str(skipframes)
             + " frames")

    slicedtraj = traj[::skipframes]
    c = CompressionData(slicedtraj, bincount)
    return c

def gen_compressor(topofile, trajfile, bincount, skipframes_input, verbose=True):
    # Our input file is a Desmond CMS--we need to use VMD to convert it to a PDB
    # because MDTraj can't read CMS (and PyMol conversion loses unit cell info)

    # Topofile and trajfile are filenames
    # bincount is an integer (number of bins)
    # skipframes_input is an input representing the number of skipframes. It can be
    # a string (in which case it is processed according to the helpdoc in main()
    # below, or it can be an int, in which case it is treated as a number of frames

    # Normalize the PDB file with a VMD script. This is useful if the input is 
    # some other file format (e.g. cms) but also is needed for some PDB files
    # to normalize the residue numbers (e.g. some files use hex, some use
    # base-36 like excel, and some use extender notation
   
    topofile = normalize_topofile(topofile)
    traj = md.load(trajfile, top=topofile).remove_solvent()
    return gen_compressor_from_mdtraj(traj, bincount, skipframes_input, verbose)

def gen_compressor_desmondir(path):
    pass

if __name__ == '__main__':
    skipframe_calc = False
    try:
        topofile = sys.argv[3]
        trajfile = sys.argv[4:]
        bincount = int(sys.argv[1])
        skipframes = sys.argv[2]
    except:
        print("Usage:%s <#bins> <#frameskip> <path-to-topo> <path-to-traj> [additional traj]")
        print("Topo file: describes connectivity of molecule, e.g. PSF, PDB")
        print("Traj file: describes positions of atoms, e.g. DCD, TRJ")
        print("Frameskip can be an integer, to describe a number of frames, or a " +
              "floating point number preceeded by 't' to indicate a minimum gap in " +
              "nanoseconds. E.g. '10' is 10 frames, while 's0.01' is at least a " +
              "0.01ns gap between frames")
        sys.exit(1)


    d = gen_compressor(topofile, trajfile, bincount, skipframes)
    print("Ratio is " + str(d.get_compression_ratios()))

    with open('ratio.txt','w') as outf:
        outf.write(str(d.get_entropy()))
