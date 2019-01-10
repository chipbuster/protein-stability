#!/opt/anaconda/anaconda3/bin/python
import numpy as np
import pickle
from numba import jit, stencil
import pickle

import spintolinear

# Ising Model simulator with both single-state and Wolf cluster flips. No
# external magnetic field accounted for.

# A spingrid is given by a 2D array of +/-1 values. The actual grid is 2 smaller
# than the literal size of the grid because this makes it easier to work with 
# numba stencils--the outer edge of values is always zeroed.

J = 1      # For now
kB = 1     # Okay this is getting silly
T = (-2 * J) / (kB * np.log(0.5)) * 0.8      # what convenient numbers!

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

@jit
def discard_borders(spingrid):
    """Discard the spingrid's borders and return an array of size (n-2,m-2)"""
    (x,y) = np.shape(spingrid)
    return spingrid[1:x-1,1:y-1]

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
    
       Returns the new spins, whether the flip was successful or not,
       along with the energy of the "new" spingrid. The new spins may or may
       not be a copy, depending on the return_copy parameter.
    """
    (x_max, y_max) = spingrid_size(spingrid)
    x = np.random.randint(1,x_max+1)
    y = np.random.randint(1,y_max+1)

    assert spingrid[x,y] == 1 or spingrid[x,y] == -1

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

@jit
def gen_neighbors(maxes, site):
    """Generate valid neighbors for a spingrid site given the size of the 
       spingrid (not size of backing array)"""
    (xmax,ymax) = maxes
    (x,y) = site

    if x + 1 <= xmax:
        yield (x+1,y)
    if y + 1 <= ymax:
        yield (x,y+1)
    if x - 1 >= 1:
        yield (x-1,y)
    if y - 1 >= 1:
        yield (x,y-1)

@jit
def wolff_cluster_find(spingrid, site):
    """Execute the Wolff cluster-growth algorithm by flipping the site and
    potentially aggregating its neighbors."""

    # Note: assume cluster flip index is in (with border) coordinates, e.g.
    # site = (1,1) corresponds to corner of actual spingrid
    (x,y) = site
    sg_max = spingrid_size(spingrid)

    # A set of sites that need to flip and were discovered more than one iter ago
    old_toflip = set()
    new_toflip = set()
    new_toflip.add(site)

    # While we still have eligible sites
    while new_toflip:
        for s in list(new_toflip):
            old_toflip.add(s)
            new_toflip.remove(s)
            for (xn,yn) in gen_neighbors(sg_max,s):
                n = (xn,yn)
                # We've already flipped this cluster
                if n in old_toflip or n in new_toflip:     
                    continue
                #This spin not eligible under Wolff
                if spingrid[xn,yn] != spingrid[x,y]: 
                    continue
                pr = np.random.rand()
                # Spin rejected by MC algorithm
                if pr > 1 - np.exp(-2*J / (kB * T)):
                    continue
                # Spin added to cluster
                new_toflip.add(n)

    return old_toflip

@jit
def ising_wolff_flip(spingrid, grid_energy,return_copy=False):
    """Flip an entire cluster via the Wolff algorithm.
    
       Returns the new spins (may or may not be a full copy), 
       along with the energy of the "new" spingrid.
    """
    (x_max, y_max) = spingrid_size(spingrid)
    x_seed = np.random.randint(1,x_max+1)
    y_seed = np.random.randint(1,y_max+1)

    cluster = wolff_cluster_find(spingrid,(x_seed,y_seed))

#    print("Flipping " + str(len(cluster)) +  " spins")

    for(xs,ys) in cluster:
        spingrid[xs,ys] = -spingrid[xs,ys]

    final_energy = calc_energy_grid(spingrid)

    if return_copy:
        return (np.copy(spingrid), final_energy)
    else:
        return final_energy

def run_ising_model(initial_grid, nsamps, step, burnin, method):
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
    if method == "ising":
        for i in range(nsamps):
            # Run <step> steps to get an independent sample
            for _ in range(step):
                energy = ising_site_flip(grid, energy, return_copy=False)
            
            # Get a sample and store it in our statelist
            (state, energy) = ising_site_flip(grid, energy, return_copy=True)
            statelist.append(state)
    elif method == "wolff":
        for i in range(nsamps):
            # Run <step> steps to get an independent sample
            for _ in range(step):
                energy = ising_wolff_flip(grid, energy, return_copy=False)
            
            # Get a sample and store it in our statelist
            (state, energy) = ising_wolff_flip(grid, energy, return_copy=True)
            statelist.append(state)
    else:
        raise ValueError("Valid methods for Ising model are 'ising' and 'wolff'")

    return statelist

def linearize_frames(frames, layout, size=8):
    """Create a raw data buffer from a series of frames.

    layout describes the process for turning a 2D sequence into a 1D sequence.

        Possible values:
           - random: Create a random (but consistent per-frame) mapping
           - row: map by row-major order
           - col: map by column-major order
           - spacefill: use a space filling curve (must be a square grid)
           - temporal: not yet implemented
    """
    (width,height) = spingrid_size(frames[0])
    numEntries = np.size(frames)
    linearized = np.empty(numEntries)
    cur = 0    # Index into linearized array that indicates current write loc

    # If we need a custom mapping, create it here.
    if layout == "random":
        mapping = spintolinear.gen_random_map((width,height))
    elif layout == "spacefill":
        if width != height:
            raise ValueError("Grid must square to use spacefilling linearization!")
        mapping = spintolinear.gen_hcurve(width) #Width = height here
    elif layout == "temporal":
        raise NotImplementedError("Temporal mapping is not yet implemented")

    for frame in (discard_borders(f) for f in frames):
        pass

    pass

if __name__ == '__main__':
    print(T)

    x = create_spingrid(50)
    states = run_ising_model(x,5000,10000,100000,'ising')
    with open("isingmodel-site.pkl",'wb') as pklfile:
        pickle.dump(states,pklfile)

    x = create_spingrid(50)
    states = run_ising_model(x,5000,1000,10000,'wolff')
    with open("isingmodel-wolff.pkl",'wb') as pklfile:
        pickle.dump(states,pklfile)
