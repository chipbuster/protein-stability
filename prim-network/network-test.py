import numpy as np
import scipy as sp
import scipy.sparse as spsp
import math
import random

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
    hess = spsp.coo_matrix((3 * npoints, 3 * npoints))
    mixd = spsp.coo_matrix((3 * npoints, nedges))

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
        mixd[3*idx1,i] = localderiv
        midx[3*idx0,i] = localderiv

        # Compute Hessian
        

    return (energy, deriv, hess, mixd)

def relaxConfig(c):
    pass

def main():
    pos = np.array([[2.0,0,0],[-2.0,0,0],[0.0,2.0,0.0]], dtype=float)
    edges = np.array([[0,1],[1,2],[0,2]], dtype=int)
    restlens = np.array([1.0,1.0,1.0], dtype=float)

    config = Config(pos,edges,restlens)

    testStruct = compEnergy(config)

    e1 = testStruct[0]

    print(testStruct[0])
    print(testStruct[1])

    delta = 0.0001

    addGrad(config, delta * testStruct[1])

    print("df.df is " + str(np.dot(testStruct[1], delta * testStruct[1])))

    testStruct = compEnergy(config)
    print("After addition of " + str(delta) +  " energy is " + str(testStruct[0]))
    print("Difference is " + str(testStruct[0] - e1))

if __name__ == '__main__':
    main()