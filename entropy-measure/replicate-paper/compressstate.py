# This module determines the compression ratio of a given state.
import numpy as np
import lzma

import common

lzmaFilters = [
    {"id": lzma.FILTER_LZMA2, "preset": 7 | lzma.PRESET_EXTREME},
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
        return np.digitize(self.states, bins)

    def gen_zeros_data(self):
        """Generate the all-zeros string for compression ratio measurements"""
        if self.cachedC0 is None:
            self.cachedC0 = np.zeros(np.shape(self.bindata), dtype = self.dtype)
        return self.cachedC0

    def gen_random_data(self):
        """Generate the random string for compression ratio measurements"""
        if self.cachedC1 is None:
            self.cachedC1 = common.parallel_choice(self.nbins, size=np.size(self.bindata),
                                      p=None)
        return self.cachedC1

    def size_data(self):
        if self.cachedCd is None:
            self.cachedCd = self.compressed_data_size(self.bindata)
        return self.cachedCd

    def size_zeros(self):
        return self.compressed_data_size(self.gen_zeros_data())

    def size_random(self):
        return self.compressed_data_size(self.gen_random_data())

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
        print("Computing compression ratios. Please be patient!")
        C_d = self.size_data()
        print("Completed phase data compression")
        C_0 = self.size_zeros()
        C_1 = self.size_random()
        print("Completed random data compression")
        eta = (C_d - C_0) / (C_1 - C_0)

        return eta

    def get_entropy(self):
        eta = self.get_compression_ratios()
        return eta * nbins
