#pragma once

#include <Eigen/Core>
#include <Eigen/Dense>
#include <vector>

enum TraceType { RING, CHAIN };

using Eigen::Matrix;

// HDF5 stores data in row-major order. It is simpler to read in contiguous data
// than it is to
using Topology = Matrix<int, Eigen::Dynamic, 2, Eigen::RowMajor>;
using Frame = Matrix<double, Eigen::Dynamic, 3, Eigen::RowMajor>;
using TimeTrace = std::vector<Frame>;

// An elastic network trace. Includes connectivity and positional data
struct ENTrace {
  // Construct from HDF5 file
  ENTrace(const char *filename, const char *datapath, TraceType t);

  Frame &getFrame(int index);
  Frame getFrameZeroMean(int index);
  const Topology &getTopo() const { return topo; }
  int length() const { return geom.size(); }

private:
  // Assumption: topology does not change over the course of the simulation
  Topology topo;
  TimeTrace geom;

  // VERY Unsafe! Check and double-check arguments!
  TimeTrace initialize_geom_from_raw(double *rawbuf, int natoms,
                                     int ntimesteps);
};
