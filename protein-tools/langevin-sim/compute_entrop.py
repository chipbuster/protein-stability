import sys
import numpy as np
sys.path.append(
    '/home/chipbuster/codes/protein-stability/protein-tools/trajectoryanalysis'
)

from compressstate_ic import *

if __name__ == "__main__":
    dat = np.loadtxt(sys.argv[1])
    dat = dat.T
    c = CompressionData(dat, 11)
    print(str(c.get_compression_ratios()))
