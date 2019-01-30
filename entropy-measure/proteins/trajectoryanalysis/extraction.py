#!/usr/bin/env python3

import numpy as np
from MDAnalysis import Universe 
from MDAnalysis.analysis.dihedrals import Dihedral

# Note: return type from Dihedral in 0.19.3 is a numpy array of size 
# nframes x natoms-1. This contains all the dihedral angles for the full
# trajectory, not just the current frame (which is sort of counter to how)
# the rest of the library works.

def get_phi(univ):
    """Given MDAnalysis Universe, return sequence of phi angles."""
    prot = univ.select_atoms("protein")
    phiAtms = [ r.phi_selection() for r in prot.residues[1:] ]
    # Need to filter out beginning residue?
    R = Dihedral(phiAtms).run()
    return R.angles

def get_psi(univ):
    """Given MDAnalysis Universe, return sequence of psi angles."""
    prot = univ.select_atoms("protein")
    psiAtms = [ r.psi_selection() for r in prot.residues[:-1] ]
    # Need to filter out beginning residue?
    R = Dihedral(psiAtms).run()
    return R.angles

def convert_IC(univ):
    """Extract a series of frames from an MD trace in internal coordinates.
    
    Returns a numpy array of internal-coordinate traces. The trace at interval
    i is in retval[i,:], and for a protein with N frames and M residues, the
    total size should be (N, 2M).
    """
    phiVals = get_phi(univ)
    psiVals = get_psi(univ)

    # Weave into double array of phi/psi timesteps. Layout should be
    #[[ psi0, phi1, psi1, phi2, psi2, ......, phiN-2, psiN-2, phiN-1 ] for t = 0
    # [ psi0, phi1, psi1, phi2, psi2, ......, phiN-2, psiN-2, phiN-1 ] for t = 1
    #] etc. etc.

    assert np.shape(phiVals) == np.shape(psiVals)
    (nframes, nres) = np.shape(psiVals)

    configs = []
    for frameN in range(nframes):
        frameConf = []
        for resN in range(nres):
            frameConf.append(psiVals[frameN,resN])
            frameConf.append(phiVals[frameN,resN])
        configs.append(frameConf)

    return np.array(configs)