import sys, os
import numpy as np
import mdtraj as md

mydir = os.path.dirname(os.path.realpath(__file__))
sys.path.append(os.path.join(mydir, ".."))

from simdata import *

try:
    outFilename = sys.argv[1]
    outDatapath = sys.argv[2]
    topofile = sys.argv[3]
    trajfiles = sys.argv[4:]
except:
    print(
        """Takes a MD simulation and writes it to a HDF5 datastore specified by the
first two arguments. This function can read any formats that mdtraj can, incl.
OpenMM traces, DCDs, and the clickme.dtr format used by Desmond."""
    )
    print(
        "\nUsage: %s <path-to-hdf5> <hdf5 datapath> <path-to-topo> <path-to-traj> [additional traj]"
    )
    print("HDF5 Path: Filesystem path to an HDF5 file. Will be created if not present.")
    print("HDF5 Datapath: Path to dataset within HDF5 file.")
    print("Topo file: describes connectivity of molecule, e.g. PSF, PDB")
    print("Traj file: describes positions of atoms, e.g. DCD, TRJ")
    sys.exit(1)

traj = md.load(trajfiles, top=topofile)
max_frame = len(traj)
print("Loaded trajectory with " + str(max_frame) + " frames")

num_residues = traj[0].topology.select("name CA").size

if num_residues < 10:
    print("[WARN]: I found only {} C-alpha atoms in this protein.".format(num_residues))

# Generate a function which gives us the Ca atoms and checks for frameshift errors
def c_alpha_pos(trajectory, frameindex):
    """Get the xyz positions of the Calpha atoms at the frame index"""
    i = frameindex
    ca_atom_index = trajectory[i].topology.select("name CA")
    assert sorted(ca_atom_index)
    outputs = trajectory.xyz[i, ca_atom_index, :]
    assert outputs.shape == (num_residues, 3)
    return outputs


# Setup is finished. Let's generate an HDF5 file to write stuff to. Things will
# be in row-major order--let's set it so that we can access a single frame by
# reading a contiguous block
outputShape = (max_frame, num_residues, 3)
h5file = InputData(outFilename, outDatapath).from_shape(outputShape)

for i in range(max_frame):
    h5file.data[i, :, :] = c_alpha_pos(traj, i)

h5file.finalize()
