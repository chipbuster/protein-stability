import h5py
import numpy as np

# See markdown file in this folder for descriptions of these datatypes


class SimData:
    def __init__(self, filepath, datapath):
        self.data = None  # Data as a numpy buffer
        self.shape = None  # Shape of data buffer
        self.filepath = filepath  # Path to the HDF5 file on the host filesystem
        self.datapath = datapath  # Path to the data within the HDF5 dataset
        self.h5File = None  # H5PY file handle

    def from_file(self):
        self.h5File = h5py.File(self.filepath)
        self.data = self.h5File[self.datapath]
        self.shape = np.shape(self.data)

    def from_numpy(self, data, attrs):
        self.h5File = h5py.File(self.filepath)
        self.data = self.h5File.create_dataset(self.datapath, data=data)
        self.shape = np.shape(self.data)
        for k in attrs:
            self.data.attrs[k] = attrs[k]

    def finalize(self):
        self.h5File.close()

    def __enter__(self):
        # If the file is already opened, we don't need to do anything here. This
        # corresponds to e.g. `with SimData(args).from_numpy(more_args) as f`
        if self.h5File is None:
            self.from_file()
        return self

    def __exit__(self, type, value, traceback):
        self.finalize()


class InputData(SimData):
    def __init__(self, filepath, datapath):
        SimData.__init__(self, filepath, datapath)
        self.datapath = self.datapath + "/inputdata"

class ParameterizedData(SimData):
    def __init__(self, filepath, datapath):
        SimData.__init__(self, filepath, datapath)
        self.datapath = self.datapath + "/parameterized"

class BinnedData(SimData):
    def __init__(self, filepath, datapath):
        SimData.__init__(self, filepath, datapath)
        self.datapath = self.datapath + "/binned"
