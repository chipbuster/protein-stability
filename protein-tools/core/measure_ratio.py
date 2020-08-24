import numpy as np
import lzma
import struct
import sys
import h5py

from simdata import BinnedData, ParameterizedData, InputData

# Note: ideally this would be Julia for full compatibility, but since Julia does
# not have good interfaces to compression libraries currently, and most of this
# code is just wrappers around C anyways, this doesn't matter *so* much.

# Compression parameters
lzmaFilters = [
    {"id": lzma.FILTER_LZMA2, "preset": 9 | lzma.PRESET_EXTREME},
]

# A class for keeping track of state info for compression
class CompressionData:
    def __init__(self, binned_data, nskip=1):
        self.bincount = binned_data.data.attrs["nbins"]
        self.dtype = self.calc_min_data_type(self.bincount)

        self.binned = binned_data.data[::nskip,:]
        self.ndof = np.size(self.binned[0])

        self.cachedCd = None
        self.cachedC0 = None
        self.cachedC1 = None

    def calc_min_data_type(self, bincount):
        if bincount < 2 ** 8:
            return np.uint8
        elif bincount < 2 ** 16:
            return np.uint16
        elif bincount < 2 ** 32:
            return np.uint32
        elif bincount < 2 ** 64:
            return np.uint64
        else:
            raise ValueError("Requested more than 2^64 bins--you might be hosed.")

    def gen_bin_data(self):
        """Generate binary form of input data"""
        data = np.array(self.binned, dtype=self.dtype)
        return data.tobytes()

    def gen_zeros_data(self):
        """Generate the all-zeros string for compression ratio measurements"""
        data = np.zeros(np.shape(self.binned), dtype=self.dtype)
        return data.tobytes()

    def gen_random_data(self):
        """Generate the random string for compression ratio measurements"""
        data = np.random.randint(
            low=0,
            high=np.max(self.binned),
            size=np.shape(self.binned),
            dtype=self.dtype,
        )
        return data.tobytes()

    def size_data(self):
        if self.cachedCd is None:
            self.cachedCd = self.compressed_data_size(self.gen_bin_data())
        return self.cachedCd

    def size_zeros(self):
        if self.cachedC0 is None:
            self.cachedC0 = self.compressed_data_size(self.gen_zeros_data())
        return self.cachedC0

    def size_random(self):
        if self.cachedC1 is None:
            self.cachedC1 = self.compressed_data_size(self.gen_random_data())
        return self.cachedC1

    def compressed_data_size(self, obj):
        """Get size of compressed data. Input needs to be a bytearray."""
        compressedStateData = lzma.compress(
            obj,
            format=lzma.FORMAT_RAW,
            check=lzma.CHECK_NONE,
            preset=None,
            filters=lzmaFilters,
        )
        return len(compressedStateData)

    def get_compression_ratios(self):
        """Compute compression ratio of data in system."""
        C_d = self.size_data()
        C_0 = self.size_zeros()
        C_1 = self.size_random()
        eta = (C_d - C_0) / (C_1 - C_0)

        return eta

    def get_entropy(self, mapping="linear"):
        """Compute entropy with given mapping from compression ratio to value."""

        if mapping == "linear":
            return self.get_compression_ratios() * self.ndof * np.log2(self.bincount)


## Simple oneshot functions to compute based on a filepath. Used to ease some
# parts of Pycall.jl interop
def compute_entropy(filepath, datapath, nskip=1):
    data = BinnedData(filepath, datapath).from_file()
    c = CompressionData(data, nskip)
    return c.get_entropy()


def compute_ratio(filepath, datapath, nskip=1):
    data = BinnedData(filepath, datapath).from_file()
    c = CompressionData(data, nskip)
    return c.get_compression_ratios()

