#!/opt/anaconda/anaconda3/bin/python
import numpy as np
from numba import jit, stencil

# Ising Model simulator with both single-state and Wolf cluster flips. No
# external magnetic field accounted for.

# A spingrid is given by a 2D array of +/-1 values. The actual grid is 2 smaller
# than the literal size of the grid because this makes it easier to work with 
# numba stencils--the outer edge of values is always zeroed.

J = 1      # For now
T = 1      # what convenient numbers!

@stencil(cval=0)
def zero_borders(spingrid):
    """A really dumb way to zero out edge cells in a spingrid"""
    ## Access spingrid values to trick numba into zeroing edges
    rt = spingrid[1,0]
    lt = spingrid[-1,0]
    up = spingrid[0,-1]
    dn = spingrid[0,1]
 
    return spingrid[0,0] + 0 * (rt + lt + up + dn)

def create_spingrid(size):
    if type(size) == int:
        sz = (size + 2, size + 2)
    elif type(size) == tuple:
        (x,y) = size
        sz = (x + 2, y + 2)
    else:
        raise ValueError("Spingrid size must be integer or 2-tuple of dims")
    spin = np.random.randint(low=0,high=2,size=sz,dtype=np.byte)
    return 2 * spin - 1   # Map (0,1) to (-1,+1)


@stencil(cval=0)
def calc_energy_site(spingrid):
    rt = J * spingrid[1,0] *  spingrid[0,0]
    lt = J * spingrid[-1,0] * spingrid[0,0]
    up = J * spingrid[0,-1] * spingrid[0,0]
    dn = J * spingrid[0,1] *  spingrid[0,0]
    return rt + lt + up + dn

# Calculate energy of a grid. H = -sum_(neighbor pairs) J_ij * spin(i) * spin(j)
@jit
def calc_energy_grid(spingrid):
    return np.sum(calc_energy_site(spingrid))

x = create_spingrid((5,5))
print(x)

x = zero_borders(x)
print(x)

print(calc_energy_site(x))

energy = calc_energy_grid(x)
print(energy)