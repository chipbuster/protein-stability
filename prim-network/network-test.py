import sys
import pickle

import numpy as np
import scipy as sp
import scipy.sparse as spsp
import scipy.sparse.linalg
import math
import random
import numba

from copy import deepcopy

from typing import List, Tuple
from collections import namedtuple

import matplotlib.pyplot as plt

npts = 10
ed_per_pt = 3  # Avg. num. springs per anchor point.
avgdist_unit = 1.0
#avgdist_unit = 0.661707182 # From https://math.stackexchange.com/q/1976842/120052

debugMode = False

Config = namedtuple("Config", ["pos","edges","restlens"])

def addGrad(c, gradient):
    """ Add gradient to c.pos """
    assert np.size(c.pos) == np.size(gradient)

    for i in range(np.shape(c.pos)[0]):
        c.pos[i,:] += gradient[3*i : 3*i + 3]
    return

def sampleEdges(npoints) -> List[Tuple[int]]:

    edgeList = []
    nedges = ed_per_pt * npoints
    maxedges = (npoints * (npoints - 1)) / 2
    nedges = nedges if nedges < maxedges else maxedges
   
    while len(edgeList) < nedges:
        x = random.randint(0,npoints-1)
        y = random.randint(0,npoints-1)
        (x,y) = (x,y) if x < y else (y,x)

        if x == y or [x,y] in edgeList:
            continue
        else:
            edgeList.append([x,y])

    assert len(set([ (x[0],x[1]) for x in edgeList])) == len(edgeList)
    
    return np.array(edgeList)

# TODO: If we're doing positions on unit cube, is this the right rest length?
def sampleRestLengths(nedges:int) -> List[float]:
    """ Return random lengths in [0.0,1.0) """
    return np.array([ random.random() for j in range(nedges) ])

def compEnergy(c):
    """ A wrapper around a JITted energy computation function. """

    npoints = c.pos.shape[0]
    nedges = c.edges.shape[0]

    (E,d,hessList,mixdList) = compEnergyCore(c)

    hessRows = [x[0] for x in hessList]
    hessCols = [x[1] for x in hessList]
    hessEntries = [x[2] for x in hessList]

    mixdRows = [x[0] for x in mixdList]
    mixdCols = [x[1] for x in mixdList]
    mixdEntries = [x[2] for x in mixdList]


    hess = spsp.coo_matrix((hessEntries, (hessRows, hessCols)), dtype=np.float64,
                            shape=(3 * npoints, 3 * npoints)).tocsr()
    mixd = spsp.coo_matrix((mixdEntries, (mixdRows, mixdCols)), dtype=np.float64,
                          shape=(3 * npoints, nedges)).tocsr()

    return (E, d, hess, mixd)

@numba.jit(cache=True)
def compEnergyCore(c):
    """ Given Config c, return a tuple with the following elements:

        - energy : energy of the configuration (float)
        - deriv : first-order differential of energy w.r.t point positions
                          (np.array)
        - hess  : hessian matrix, all second-order derivatives of energy 
                          w.r.t. point positions (spsp.coo_matrix)
        - mixd  : mixed derivatives, differential of individual edge's energy 
                          w.r.t endpoints (spsp.coo_matrix)

        in the struct (e, deriv, hess, mixd)
    """

    npoints = c.pos.shape[0]
    nedges = c.edges.shape[0]
    deriv = np.zeros(3 * npoints)

    # Use lists to store triples for coo_matrix construction (basically the
    # same thing as Eigen's setFromTriplets function)
    # hess = spsp.coo_matrix((3 * npoints, 3 * npoints))
    # mixd = spsp.coo_matrix((3 * npoints, nedges))
    hessList = []
    mixdList = []

    energy = 0.0

    for i in range(nedges):
        idx0 = c.edges[i,0]
        idx1 = c.edges[i,1]

        # Compute contribution to energy, 0.5 k x^2
        delta = c.pos[idx1,:] - c.pos[idx0,:]
        energy += 0.5 * (np.linalg.norm(delta) - c.restlens[i]) ** 2

        # Compute contrib to deriv from this spring
        localderiv = (np.linalg.norm(delta) - c.restlens[i]) * (delta / np.linalg.norm(delta))  # Need to normalize?

        deriv[3*idx0:3*idx0 + 3] -= localderiv
        deriv[3*idx1:3*idx1 + 3] += localderiv

        # Compute mixed derivatives (spring-local energy derivative)
        for j in range(3):
            mixdList.append((3*idx0 + j, i, -localderiv[j]))
            mixdList.append((3*idx1 + j, i, localderiv[j]))

        # Compute Hessian
        dOP = np.outer(delta,delta)   # Outerproduct of delta and delta
        dSN = np.dot(delta,delta)     # squared norm of delta
        d2N = np.linalg.norm(delta,2) # 2 norm of delta
        eye = np.identity(3)
        localHess = dOP / dSN + (d2N - c.restlens[i]) * (eye - (dOP / dSN)) / d2N

        # Mirror copies of Hessian into relevant areas in the global Hessian
        for j in range(3):
            for k in range(3):
                hessList.append((3 * idx0 + j, 3 * idx0 + k, localHess[j,k]))
                hessList.append((3 * idx1 + j, 3 * idx1 + k, localHess[j,k]))
                hessList.append((3 * idx0 + j, 3 * idx1 + k, -localHess[j,k]))
                hessList.append((3 * idx1 + j, 3 * idx0 + k, -localHess[j,k]))

        # Generate sparse matrices from hessList and mixdList

    return (energy, deriv, hessList, mixdList)

def relaxConfig(c):
    (E, deriv, hess, mixd) = compEnergy(c)

    # Create view of positions as a giant vector -- shares underlying memory
    pos = c.pos
    posVec = pos.view().reshape((pos.size,))

    # Relax via Newton's Method
    # N.B. I don't have a Sparse QR solver and the direct solver runs into
    # numerical issues, so I'm using an iterative method here
    while np.linalg.norm(deriv) > 1e-5:
        (newPos, info) = scipy.sparse.linalg.gmres(hess,deriv)

        if debugMode:
            print(E, info)
            print(np.linalg.norm(deriv))

        if np.any(np.isnan(newPos)) :
            print("[ERROR]: Numerical solve lead to NaNs")
            for r in hess.todense():
                print(r)
            return

        # Map change onto config's positions
        posVec -= newPos

        # Recompute Hessian
        (E, deriv, hess, mixd) = compEnergy(c)

def alignConfig(dst, src):
    """ Output (R,t) that aligns dst to src with Orthogonal Procrustes

        Output rotation matrix R that minimizes ||R * src - dst||.
        This is done by an Orthogonal Procrustes alignment.
    """

    dPos = dst.pos
    sPos = src.pos

    dstCentroid = np.sum(dPos, axis=0) / np.shape(dPos)[0]
    srcCentroid = np.sum(sPos, axis=0) / np.shape(sPos)[0]

    assert np.shape(dstCentroid) == (3,)

    trans = dstCentroid - srcCentroid

    # Compute Procrustes with Scipy's built-in algorithm. If the determinant
    # is negative (since SP allows this), manually correct it using the SVD.
    # This is a 3x3 matrix so the computational cost should be minimal

    (R, scaling) = sp.linalg.orthogonal_procrustes(sPos, dPos)
    if np.linalg.det(R) < 0:
        (u,s,vt) = np.linalg.svd(R)
        s[-1] = -s[-1]           # Flip the smallest singular value
        R = u @ np.diag(s) @ vt  # Reconstruct R

    assert np.linalg.det(R) > 0

    return (R,trans)

def dumpConfig(ident, Cref, Ccut):
    """Dump config pair to a file named by ident"""
    pickle.dump((Cref, Ccut), open("Config-" + str(ident) + ".pkl", 'wb'))

def main(args):

    inpSeed = -1

    if len(args) > 1:
        try:
            inpSeed = int(args[1])
        except ValueError:
            print("Attempted to set invalid seed.")

    if inpSeed < 0:
        print("Using randomly-generated seed")
        seed = random.randint(0,2**32)
    else:
        seed = inpSeed

    print("System seed is " + str(seed))
    random.seed(seed) # Hooray for consistency!
    np.random.seed(random.randint(0,2**32))

    npoints = npts

    # Set up inputs to create our config
    cpos = (np.random.rand(npoints, 3) - 0.5) * 2
    ced = sampleEdges(npoints)
    nedges = len(ced)
    crest = sampleRestLengths(nedges)
    C = Config(cpos, ced, crest)

    relaxConfig(C)

    # Sever each spring, then relax the resultant network. Align each cut
    # network, then store the distances
    cutDistances = np.zeros(nedges)
    for j in range(nedges):
        # Cut edge j
        if debugMode:
            print("Cut edge " + str(j))

        newedges = np.delete(C.edges, j, axis=0)
        newrestlens = np.delete(C.restlens, j)

        C2 = Config(deepcopy(C.pos), newedges, newrestlens)

        relaxConfig(C2)

        (R, t) = alignConfig(C,C2)

        C2posAlign = (R.T @ (C2.pos + t).T).T
        Caligned = Config(C2posAlign, newedges, newrestlens)

        diff = Caligned.pos - C.pos

        cutDistances[j] = np.linalg.norm(diff, ord='fro')

        if cutDistances[j] > 0.75:
            ident = str(seed) + "-" + str(j)
            dumpConfig(ident, C, C2)
            print("Pickled configuration with spring " + str(j) + " cut")

    # Some configs seem to have *really* big changes. Not sure what causes this
    numBigChange = np.size(cutDistances[cutDistances > 1.0])
    if numBigChange > (nedges / 4):
        print("[WARN]: Got " + str(numBigChange) + " major changes in this config.")
        print("Not sure if output is sane. Random seed was " + str(seed))

    # Sensitivity analysis
    predictedDistances = np.zeros(nedges)
    (E,deriv,hess,mixd) = compEnergy(C)

    for j in range(nedges):
        springDeriv = mixd[:,j].todense()
        dQ, status = scipy.sparse.linalg.bicgstab(A=hess,b=springDeriv)
        predictedDistances[j] = np.linalg.norm(dQ)

    # Strain analysis
    strain = np.zeros(nedges)
    for j in range(nedges):
        (e1,e2) = C.edges[j]
        springLength = np.linalg.norm(C.pos[e1,:] - C.pos[e2,:])
        strain[j] = (springLength - C.restlens[j]) / C.restlens[j]

    for j in range(nedges):
        print("%d %f %f %f" % (j, cutDistances[j], predictedDistances[j], strain[j]))

    # Plot based on (sorted) actual distance
    dists = [ (cutDistances[j], predictedDistances[j], strain[j]) for j in range(nedges)]
    sDists = sorted(dists)
    fig = plt.figure()
    plt.plot(range(nedges), [x[0] for x in sDists], c='red', marker='o')
    plt.plot(range(nedges), [x[1] for x in sDists], c='green', marker='o')
#    plt.plot(range(nedges), [x[2] for x in sDists], c='blue', marker='o')
    plt.show(block=True)

if __name__ == '__main__':
    main(sys.argv)