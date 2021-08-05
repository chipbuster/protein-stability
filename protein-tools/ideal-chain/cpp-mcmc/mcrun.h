#include <cstdint>
#include <random>
#include <string>

#include <Eigen/Core>
#include <Eigen/Dense>

#include "H5Cpp.h"
#include "xoroshiro128plus.h"

using Eigen::MatrixXd;
using Eigen::MatrixXf;
using Eigen::VectorXd;

struct MCRunSettings {
  double lo;
  double hi;
  double gaussWidth;
  int64_t numAngles;
  int64_t numSteps;
  int64_t skipPerStep;

  MCRunSettings(int numAngles, int64_t numSteps, int64_t skipsPerStep,
                double lo, double hi)
      : lo{lo}, hi{hi}, gaussWidth{(hi - lo) / (numAngles * numAngles)},
        numAngles{numAngles}, numSteps{numSteps}, skipPerStep{skipsPerStep} {}

  // Tag the given HDF5 dataset with these attributes
  void tagDataset(H5::DataSet &dset) const;
};

/** A class which is used to amortize H5Write costs. It assumes the data is
 * added one vector at a time into the dataset. It uses an internal buffer to
 * avoid calling into HDF5 all the time. Once the buffer is exhausted, it
 * flushes all of its data into the specified HDF5 dataset. */
class MCBuffer {
  MatrixXd
      buffer; // Used to buffer writes so we don't call into HDF5 every iter

  // Used to record which column in outBuf we're recording. Updates when
  // a new column is written into the outBuf, points to first unwritten column
  int bufIndex = 0;

  // Used to record the next location for a write from outBuf to the HDF5 file.
  // A write will be of size (nAngles, bufIndex) and run from fileIndex to
  // minimum(dataset.maxcols(), fileIndex + outBuf.cols())
  int fileIndex = 0;

public:
  MCBuffer(int64_t nrow, int64_t ncol) : buffer(nrow, ncol) {}
  void recordState(const VectorXd &state, H5::DataSet &ds);
  void flush(H5::DataSet &ds);
};

class MCRunState {
  // Local buffers: none of this data should ever make it into outputs (except
  // for scratchBuf, which is used to buffer output as an implementation detail)
  VectorXd curState;
  VectorXd scratchBuf; // Used when internal computations require scratch
  int bufIndex = 0;

public:
  // Settings which are written into the output as attributes
  MCRunSettings settings;
  int64_t accept;
  int64_t reject;

  // Variables used for HDF5 output
  H5std_string hdf5Filename;
  H5std_string datasetName = "angles";
  H5::H5File outfile;
  H5::DataSet ds;

  // Variables used for random number generation
  std::random_device r;
  uint64_t seed{r()};
  xoroshiro128plus e2;
  std::normal_distribution<double> normal_dist;

  MCRunState(const MCRunSettings &settings, const std::string &filename);
  MCRunState(const std::string &filename);

  bool takeStep(VectorXd &curstate, VectorXd &scratch,
                xoroshiro128plus &randEngine,
                std::normal_distribution<double> &dist);
  bool stateIsValid(const VectorXd &state) const;

  void recordState(const VectorXd &state, int step);
  void runSimulation();
  void finalize();
};

// MCRunState deserialize(const std::string &filename);