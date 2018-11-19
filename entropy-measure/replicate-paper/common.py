import os
import numpy as np
from multiprocessing import Pool

def choice(a,b,c,d):
    np.random.seed(int.from_bytes(os.urandom(4), byteorder='little'))
    return np.random.choice(a,b,c,d)

def parallel_choice(a, size, p=None, nprocs=1):
    print(nprocs)
    pool = Pool(nprocs)
    arrlens = [len(x) for x in np.array_split(np.zeros(size), nprocs)]

    args = [(a, sz, True, p) for sz in arrlens]

    randarrs = pool.starmap(choice, args)
    return np.concatenate(randarrs, axis=0)
