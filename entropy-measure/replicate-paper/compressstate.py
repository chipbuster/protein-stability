# This module determines the compression ratio of a given state.
import numpy as np
import lzma

import common

lzmaFilters = [
    {"id": lzma.FILTER_LZMA2, "preset": 7 | lzma.PRESET_EXTREME},
]

# A class for keeping track of state info for compression
class CompressionData:
    def __init__(self,states, nbins, minval, maxval, dtype):
        self.states = states
        self.nbins = nbins
        self.minval = minval
        self.maxval = maxval
        self.bindata = self.bin_states()
        self.dtype = dtype
    
    def bin_states(self):
        """Bin floating point states into integers based off of supplied values"""
        bins = np.linspace(self.minval, self.maxval, num=self.nbins)
        return np.digitize(self.states, bins)

    def gen_zeros_data(self):
        """Generate the all-zeros string for compression ratio measurements"""
        return np.zeros(np.shape(self.bindata), dtype = self.dtype)

    def gen_random_data(self):
        """Generate the random string for compression ratio measurements"""
        return common.parallel_choice(self.nbins, size=np.size(self.bindata),
                                      p=None, nprocs=16)

    def size_data(self):
        return self.compressed_data_size(self.bindata)

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