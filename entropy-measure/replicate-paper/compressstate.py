# This module determines the compression ratio of a given state.
import numpy as np
import lzma
import struct

import common

lzmaFilters = [
    {"id": lzma.FILTER_LZMA2}, #, "preset": 7 | lzma.PRESET_EXTREME},
]

# A class for keeping track of state info for compression
class CompressionData:
    def __init__(self,states, nbins, ndof, minval, maxval, dtype):
        self.states = states
        self.nbins = nbins
        self.ndof = ndof
        self.minval = minval
        self.maxval = maxval
        self.bindata = self.bin_states()
        self.dtype = dtype

        self.cachedCd = None
        self.cachedC0 = None
        self.cachedC1 = None

    def bin_states(self):
        """Bin floating point states into integers based off of supplied values"""
        bins = np.linspace(self.minval, self.maxval, num=self.nbins)
        return np.digitize(self.states, bins, right=True)

    def gen_zeros_data(self):
        """Generate the all-zeros string for compression ratio measurements"""
        return np.zeros(np.shape(self.bindata), dtype = self.dtype)

    def gen_random_data(self):
        """Generate the random string for compression ratio measurements"""
        return common.parallel_choice(self.nbins, size=np.size(self.bindata),
                                      p=None)

    def size_data(self):
        if self.cachedCd is None:
            print("Compressing Sample Data...please be patient")
            self.cachedCd = self.compressed_data_size(self.bindata)
        return self.cachedCd

    def size_zeros(self):
        if self.cachedC0 is None:
            self.cachedC0 = self.compressed_data_size(self.gen_zeros_data())
        return self.cachedC0

    def size_random(self):
        if self.cachedC1 is None:
            print("Compressing Random Data...please be patient")
            self.cachedC1 = self.compressed_data_size(self.gen_random_data())
#            print("Random Data compressed.")
        return self.cachedC1

    def compressed_data_size(self, obj):
        """Get size of compressed data. Input needs to be a bytearray."""
        compressedStateData = lzma.compress(obj, 
                                    format=lzma.FORMAT_RAW,
                                    check=lzma.CHECK_NONE,
                                    preset=None,
                                    filters=lzmaFilters)
        return len(compressedStateData)

    def get_compression_ratios(self):
        """Compute compression ratio of data in system."""
        C_d = self.size_data()
        C_0 = self.size_zeros()
        C_1 = self.size_random()
        eta = (C_d - C_0) / (C_1 - C_0)

        return eta