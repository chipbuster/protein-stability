# Implement a sampler for a system with a finite set of states, separated by
# a certain number of quanta.

# NB: We must take care to distinguish between functions that calculate values
# for a single particle and functions that calculate values for the ensemble.
# For any system where ambiguity is possible, the suffix _one or _all will
# clarify whether this is for one or all particles.

import numpy as np
import matplotlib.pyplot as plt

import common
from compressstate import *

kB = 1

# A system with energy states at 1e, 2e, 30e, and 70e
exampleRatios = np.array([1, 2, 70, 80])

def beta(T):
    return 1.0 / (kB * T)

def calc_partition_function_one(states, T):
    """Calculate the partition function associated with a set of energy states."""
    B = beta(T)
    Z = sum(np.exp(-B * states))

    return Z

def calc_probabilities_one(states, T):
    """Calculate the probability of being in the i-th state"""
    return np.exp(-beta(T) * states) / calc_partition_function_one(states, T)

def sample_states_all(states, T, nsamp):
    """Sample the states with a Boltzmann distribution at the given temperature"""

    stateProbs = calc_probabilities_one(states, T)
    # instead of sampling the energy level, sample the energy level index.
    sample = common.parallel_choice(list(range(len(states))), size=int(nsamp)
                            , p=stateProbs)
    return sample

def calc_entropy_one(states, T):
    """Calculate the entropy of a single state at the given temperature."""

    # S = - kB \sum_i p_i \log p_i = - k_B <\log p_i>
    P = calc_probabilities_one(states, T)
    return -kB * np.sum(P * np.log(P))

def main():
    T = 100
    D = int(1e7)
    n_s = 4

    data = sample_states_all(exampleRatios, T, D)
    CDataObj = CompressionData(data, len(exampleRatios), 1,
                               0, len(exampleRatios), np.uint8)
    print("Compressed size is " + str(CDataObj.size_data()))
    print("Zero size is " + str(CDataObj.size_zeros()))
    print("Random size is " + str(CDataObj.size_random()))
    print("Compression ratio is " + str(CDataObj.get_compression_ratios()))

    eta = CDataObj.get_compression_ratios()
    entropy_estimate = eta * D * np.log(n_s)

    entropy_real = calc_entropy_one(exampleRatios, T) * D

    print("Estimated entropy is " + str(entropy_estimate) + 
                ", while real entropy (NVT) is " + str(entropy_real))
    print("Overestimate is " + str(entropy_estimate - entropy_real))

if __name__ == "__main__":
    main()