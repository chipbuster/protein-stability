# Implement a sampler for a system with a finite set of states, separated by
# a certain number of quanta.

import numpy as np

kB = 1

# A system with energy states at 1e, 2e, 30e, and 70e
exampleRatios = np.array([1, 2, 30, 70])

def beta(T):
    return 1.0 / (kB * T)

def calc_partition_function(states, T):
    """Calculate the partition function associated with a set of energy states."""
    B = beta(T)
    Z = sum(np.exp(-B * states))

    return Z

def calc_probabilities(states, T):
    """Calculate the probability of being in the i-th state"""
    return np.exp(-beta(T) * states) / calc_partition_function(states, T)

def sample_states(states, T, nsamp):
    """Sample the states with a Boltzmann distribution at the given temperature"""

    stateProbs = calc_probabilities(states, T)
    # instead of sampling the energy level, sample the energy level index.
    sample = np.random.choice(list(range(len(states))), size=int(nsamp)
                            , p=stateProbs)
    return sample