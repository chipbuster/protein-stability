#include "filereaders.h"
#include <cassert>
#include <hdf5.h> // C-style header...can't be helped, I suppose
#include <iostream>

// Construct an ENTrace from
ENTrace::ENTrace(const char *filename, const char *datapath, TraceType t) {
  hid_t infile_id = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);
  hid_t dset_id = H5Dopen2(infile_id, datapath, H5P_DEFAULT);
  hid_t dspace_id = H5Dget_space(dset_id);

  // We need to get dims and check that we actually got a 3-tensor
  hsize_t data_dims[3];
  const int ndims = H5Sget_simple_extent_dims(dspace_id, data_dims, NULL);
  assert(ndims == 3 && "Did not get 3-tensor: dataset has wrong dimensions");

  // Sanity check the dimensions we got and unpack
  const long nsteps = data_dims[0];
  const long natoms = data_dims[1];
  const long sdims = data_dims[2];
  assert(sdims == 3 && "I can only visualize 3D data, but this data is not 3D");
  if (nsteps < 100) {
    std::cout << "This dataset has " << nsteps << " timesteps. Are you sure"
              << " this is a correctly-formatted trace?" << std::endl;
  }

  long bufSize = nsteps * natoms * sdims;
  std::cout << "Requesting an array of " << bufSize << " doubles." << std::endl;
  double *data = new double[bufSize];
  int status =
      H5Dread(dset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
  if (status < 0) {
    std::cerr << "An error occurred when reading " << filename << '\n';
    std::cerr << "HDF5 status was " << status << std::endl;
  }
  this->geom = initialize_geom_from_raw(data, natoms, nsteps);

  if (t == TraceType::RING) {
    this->topo = Topology(natoms, 2);
    for (int i = 0; i < natoms - 1; i++) {
      this->topo(i, 0) = i;
      this->topo(i, 1) = i + 1;
    }
    // Manually add the final link in the ring (last atom to first atom);
    this->topo(natoms - 1, 0) = 0;
    this->topo(natoms - 1, 1) = natoms - 1;
  } else { // t == CHAIN
    this->topo = Topology(natoms - 1, 2);
    for (int i = 0; i < natoms - 1; i++) {
      this->topo(i, 0) = i;
      this->topo(i, 1) = i + 1;
    }
    // No final link
  }

  delete[] data;

  std::cout << "Loaded " << this->geom.size() << " frames of " << natoms << "x"
            << "3 each, in a " << (t == CHAIN ? "CHAIN" : "RING")
            << " configuration" << std::endl;
}

// Initialize a TimeTrace from a raw buffer and size data. Very unsafe.
TimeTrace ENTrace::initialize_geom_from_raw(double *rawbuf, int natoms,
                                            int ntimesteps) {
  std::vector<Frame> frames;
  frames.reserve(ntimesteps);

  int framesize = natoms * 3;

  for (int i = 0; i < ntimesteps; i++) {
    Frame f(natoms, 3);
    double *frame_startpos = rawbuf + i * framesize;
    for (int j = 0; j < natoms; j++) {
      double *atom_startpos = frame_startpos + j * 3;
      for (int k = 0; k < 3; k++) {
        f(j, k) = atom_startpos[k];
      }
    }
    frames.push_back(f);
  }

  return frames;
}

Frame &ENTrace::getFrame(int index) { return this->geom[index]; }

Frame ENTrace::getFrameZeroMean(int index) {
  Frame newFrame = this->geom[index];

  newFrame.rowwise() -= newFrame.colwise().mean();
  return newFrame;
}
