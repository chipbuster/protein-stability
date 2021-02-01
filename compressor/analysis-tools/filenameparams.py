# Functions to get params from filenames
import os.path as path


class FnameParams(object):
    def __init__(self, fname):
        fn = path.basename(fname)
        parts = fname.split("_")
        if parts[0] != "output":
            raise ValueError("Not an output file.")
        self.natoms = int(parts[1])
        self.ext_frac = float(parts[2])
        self.njit = int(parts[3])
        self.sorted = parts[4] == "sorted"
        self.aatype = int(parts[5][0])
        self.eedist = (self.natoms - 1) * self.ext_frac

