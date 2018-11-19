import os
import numpy as np
from multiprocessing import Pool

def choice(a,b,c,d):
    np.random.seed(int.from_bytes(os.urandom(4), byteorder='little'))
    return np.random.choice(a,b,c,d)

def parallel_choice(a, size, p=None):
    pool = Pool(16)

    args = [(a, int(size / 16 + 1), True, p) for _ in range(16)]

    randarrs = pool.starmap(choice, args)
    return np.concatenate(randarrs, axis=0)