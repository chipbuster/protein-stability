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
kB = 1     # Okay this is getting silly

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
    spin = 2 * spin - 1        # Map (0,1) to (-1,+1)
    spin = zero_borders(spin)  # Zero non-relevant cells
    return spin

def spingrid_size(spingrid):
    """Utility function for calculating the "real" size of the spingrid."""
    (x,y) = np.shape(spingrid)
    return (x-2, y-2)

@stencil(cval=0)
def calc_energy_site(spingrid):
    rt = J * spingrid[1,0] *  spingrid[0,0]
    lt = J * spingrid[-1,0] * spingrid[0,0]
    up = J * spingrid[0,-1] * spingrid[0,0]
    dn = J * spingrid[0,1] *  spingrid[0,0]
    return -(rt + lt + up + dn)

# Calculate energy of a grid. H = -sum_(neighbor pairs) J_ij * spin(i) * spin(j)
@jit
def calc_energy_grid(spingrid):
    return np.sum(calc_energy_site(spingrid))

@jit
def ising_site_flip(spingrid, grid_energy,return_copy=False):
    """Attempt to flip a site in the Ising grid.
    
       Returns a copy of the new spins, whether the flip was successful or not,
       along with the energy of the "new" spingrid.
    """
    (x_max, y_max) = spingrid_size(spingrid)
    x = np.random.randint(0,x_max)
    y = np.random.randint(0,y_max)

    spingrid[x,y] = -spingrid[x,y]

    new_energy = calc_energy_grid(spingrid)

    # Accept or reject transition based on MCMC rules.
    accept = False
    deltaE = new_energy - grid_energy
    if deltaE < 0:
        accept = True
    else:
        alph = np.random.ranf()
        acceptProb = np.exp(-deltaE / (kB * T))
        if alph < acceptProb:
            accept = True
        else:
            accept = False

    # Assign new configurations based on accept flag
    if accept:
        final_energy = new_energy
    else:
        spingrid[x,y] = -spingrid[x,y] # Reject transition and reverse siteflip
        final_energy = grid_energy

    if return_copy:
        return (np.copy(spingrid), final_energy)
    else:
        return final_energy

def run_ising_model(initial_grid, nsamps, step, burnin):
    """Run a single-flip Ising model for on <initial_grid>.
    Generate <nsamps> samples that are <step> apart in a single-site-flip
    MCMC simulation 

    Returns a list of tuples--each is a state along with its energy."""

    statelist = []

    energy = calc_energy_grid(initial_grid)
    grid = initial_grid

    # Burn the MCMC in so that we're not dealing with initialization effects
    print("Burning in Ising Grid...")
    for _ in range(burnin):
        energy = ising_site_flip(grid, energy, return_copy=False)

    print("Sampling Ising Model...")
    # Sample from grid
    for i in range(nsamps):
        # Run <step> steps to get an independent sample
        for _ in range(step):
            energy = ising_site_flip(grid, energy, return_copy=False)
        
        # Get a sample and store it in our statelist
        (state, energy) = ising_site_flip(grid, energy, return_copy=True)
        statelist.append(state)

    return statelist
        

x = create_spingrid(50)
energy = calc_energy_grid(x)
print("Energy is ",energy)

states = run_ising_model(x,50,1000,10000)