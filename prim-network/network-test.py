import numpy as np
import scipy as sp
import scipy.sparse as spsp
import math
import random
import numba

from typing import List, Tuple
from collections import namedtuple

random.seed(3) # Hooray for consistency!
ed_per_pt = 3  # Avg. num. springs per anchor point.

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
   
    while len(edgeList < nedges):
        (x,y) = random.randint(0,nedges-1)
        (x,y) = (x,y) if x < y else (y,x)

        if x == y or (x,y) in edgeList:
            continue
        else:
            edgeList += (x,y)
    
    return edgeList

# TODO: If we're doing positions on unit cube, is this the right rest length?
def sampleRestLengths(nedges:int) -> List[float]:
    """ Return random lengths in [0.0,1.0) """
    return [ random.random() for j in range(nedges) ]


@numba.jit(nopython=True)
def compEnergy(c):
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
        print(delta)
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
        hessRows = [x[0] for x in hessList]
        hessCols = [x[1] for x in hessList]
        hessEntries = [x[2] for x in hessList]

        mixdRows = [x[0] for x in mixdList]
        mixdCols = [x[1] for x in mixdList]
        mixdEntries = [x[2] for x in mixdList]

        hess = spsp.coo_matrix((hessEntries, (hessRows, hessCols)),
                                shape=(3 * npoints, 3 * npoints)).tocsr()
        mixd = spsp.coo_matrix((mixdEntries, (mixdRows, mixdCols)),
                              shape=(3 * npoints, nedges)).tocsr()

    return (energy, deriv, hess, mixd)

def relaxConfig(c):
    (E, deriv, hess, mixd) = compEnergy(c)

    # Create view of positions as a giant vector
    posVec = c.pos.view().reshape((pos.size,))

    # Relax via Newton's Method
    while np.linalg.norm(deriv) > 1e-8:
        newPos = sp.linalg.solve(hess,deriv,sym_pos=True)

        # Map change onto config's positions
        posVec -= newPos

def alignConfig(dst, src):
    """ Output (R,t) that aligns dst to src with Orthogonal Procrustes

        Output rotation matrix R that minimizes ||R * src - dst||.
        This is done by an Orthogonal Procrustes alignment.
    """

    dPos = dst.pos
    sPos = src.pos

    dstCentroid = np.sum(dPos, axis=0) / np.shape(dPos)[0]
    srcCentroid = np.sum(dPos, axis=0) / np.shape(sPos)[0]

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

def main():
    pos = np.array([[2.0,0,0],[-2.0,0,0],[0.0,2.0,0.0]], dtype=float)
    edges = np.array([[0,1],[1,2],[0,2]], dtype=int)
    restlens = np.array([1.0,1.0,1.0], dtype=float)

    config = Config(pos,edges,restlens)

    testStruct = compEnergy(config)

    e1 = testStruct[0]

    print(testStruct[0])
    print(testStruct[1])

    delta = 0.000001

    addGrad(config, delta * testStruct[1])

    print("df.df is " + str(np.dot(testStruct[1], delta * testStruct[1])))

    testStruct = compEnergy(config)
    print("After addition of " + str(delta) +  " energy is " + str(testStruct[0]))
    print("Difference is " + str(testStruct[0] - e1))

if __name__ == '__main__':
    main()