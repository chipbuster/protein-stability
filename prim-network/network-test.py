import numpy as np
import scipy as sp
import math
import random
from typing import List, Tuple

random.seed(3) # Hooray for consistency!
ed_per_pt = 3  # Avg. num. springs per anchor point.

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

def sampleRestLengths(nedges:int) -> List[float]:
    return [ random.random() for j in range(nedges) ]

def compEnergy(c, deriv, hessian, mixedderiv) -> float:
    pass

def relaxConfig(c):


def main():


if __name__ == '__main__':
    main()