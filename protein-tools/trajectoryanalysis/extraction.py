#!/usr/bin/env python3

import numpy as np
import mdtraj as md

def interleave_angles(a,b):
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
    (ax,ay) = np.shape(a)
    (bx,by) = np.shape(b)

    if ax != bx:
        raise ValueError("Inputs to interleave_angles() must have same size in "
                        +"first dimension. Got a with shape " + str(np.shape(a))
                        +" and b with shape " + str(np.shape(b)))

    c = np.empty((ax, ay + by), dtype=a.dtype)
    for j in range(ax):
        c[j,0::2] = a[j,:]
        c[j,1::2] = b[j,:]

    return c

def convert_IC(traj):
    """Extract a series of frames from an MD trace in internal coordinates.
    
    Returns a numpy array of internal-coordinate traces. The trace at interval
    i is in retval[i,:], and for a protein with N frames and M residues, the
    total size should be (N, 2M).
    """

    # Return values from calls are np.ndarray of shape (n_frames, n_phi)
    (_,phiVals) = md.compute_phi(traj)
    (_,psiVals) = md.compute_psi(traj)

    # Weave into double array of phi/psi timesteps. Layout should be
    #[[ psi0, phi1, psi1, phi2, psi2, ......, phiN-2, psiN-2, phiN-1 ] for t = 0
    # [ psi0, phi1, psi1, phi2, psi2, ......, phiN-2, psiN-2, phiN-1 ] for t = 1
    #] etc. etc.

    return interleave_angles(psiVals,phiVals)

#def get_phi_psi_from_cartesian(mat)
a =   """Convert a series of 3D snapshots into phi-psi angles

    Sometimes it is impossible (or very difficult) to get an actual MDTraj
    trace (e.g. when we're doing ENM simulations). To solve this problem, we
    provide this alternate function which takes in a set of 3D points in a
    3xNxtimesteps array and returns a set of phi-psi angles consistent with
    convert_IC
    """
    



