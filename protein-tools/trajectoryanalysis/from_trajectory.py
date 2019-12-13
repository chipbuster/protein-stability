import sys, os
import numpy as np
import mdtraj as md

mydir = os.path.dirname(os.path.realpath(__file__))
sys.path.append(os.path.join(mydir, ".."))

from core.simdata import *


def interleave_angles(a, b):
    """Interleave two arrays rowwise.

    Given arrays
          a11 a12 a13           b11 b12 b13
    a =   a21 a22 a23      b =  b21 b22 b33
          a31 a32 a33           b31 b32 b33

    return the array
        a11 b11 a12 b12 a13 b13
   c =  a21 b21 a22 b22 a23 b23
        a31 b31 a32 b32 a33 b33


    """
    (ax, ay) = np.shape(a)
    (bx, by) = np.shape(b)

    if ax != bx:
        raise ValueError(
            "Inputs to interleave_angles() must have same size in "
            + "first dimension. Got a with shape "
            + str(np.shape(a))
            + " and b with shape "
            + str(np.shape(b))
        )

    c = np.empty((ax, ay + by), dtype=a.dtype)
    for j in range(ax):
        c[j, 0::2] = a[j, :]
        c[j, 1::2] = b[j, :]

    return c


def convert_IC(traj):
    """Extract a series of frames from an MD trace in internal coordinates.
    
    Returns a numpy array of internal-coordinate traces. The trace at interval
    i is in retval[i,:], and for a protein with N frames and M residues, the
    total size should be (N, 2M).
    """

    # Return values from calls are np.ndarray of shape (n_frames, n_phi)
    (_, phiVals) = md.compute_phi(traj)
    (_, psiVals) = md.compute_psi(traj)

    # Weave into double array of phi/psi timesteps. Layout should be
    # [[ psi0, phi1, psi1, phi2, psi2, ......, phiN-2, psiN-2, phiN-1 ] for t = 0
    # [ psi0, phi1, psi1, phi2, psi2, ......, phiN-2, psiN-2, phiN-1 ] for t = 1
    # ] etc. etc.

    return interleave_angles(psiVals, phiVals)


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

print("Writing calpha trace to HDF5")
h5ifile = InputData(outFilename, outDatapath).from_shape(outputShape)

for i in range(max_frame):
    h5ifile.data[i, :, :] = c_alpha_pos(traj, i)

h5ifile.finalize()

print("Writing ramachandran angles to HDF5")
# Get the phi-psi angles and store those into the parameterized data
phipsi = convert_IC(traj)

h5pfile = ParameterizedData(outFilename, outDatapath).from_numpy(
    phipsi, {"maxval": 180.0, "type": "ramachandran"}
)
h5pfile.finalize()
