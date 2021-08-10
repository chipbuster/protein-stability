#include <cstdint>
#include <random>
#include <string>

#include <Eigen/Core>
#include <Eigen/Dense>

#include "H5Cpp.h"
#include "pcg_random.hpp"

using Eigen::MatrixXd;
using Eigen::MatrixXf;
using Eigen::VectorXd;

using RandAlgo = pcg32;

struct MCRunSettings {
  // Params are used by different kinds of simulations for different things.
  // Should be accessed through accessors in the simulation class.
  double param1;
  double param2;
  double param3;
  double gaussWidth;
  int64_t numAngles;
  int64_t numSteps;
  int64_t skipPerStep;

  MCRunSettings(int numAngles, int64_t numSteps, int64_t skipsPerStep,
                Eigen::Vector3d params, double gW)
      : param1{params(0)}, param2{params(1)}, param3{params(2)}, gaussWidth{gW},
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
  uint64_t bufIndex = 0;

  // Used to record the next location for a write from outBuf to the HDF5 file.
  // A write will be of size (nAngles, bufIndex) and run from fileIndex to
  // minimum(dataset.maxcols(), fileIndex + outBuf.cols())
  uint64_t fileIndex = 0;

public:
  MCBuffer(int64_t nrow, int64_t ncol) : buffer(nrow, ncol) {}
  void recordState(const VectorXd &state, H5::DataSet &ds);
  void flush(H5::DataSet &ds);
};

class MCRunState {
  // Local buffers: none of this data should ever make it into outputs (except
  // for scratchBuf, which is used to buffer output as an implementation detail)
protected:
  MCBuffer outBuf;
  VectorXd curState;
  VectorXd prevState;
  VectorXd scratchBuf;

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
  RandAlgo e2;
  std::normal_distribution<double> normal_dist;
  std::uniform_real_distribution<double> unif_dist;

  MCRunState(const MCRunSettings &settings, const std::string &filename);
  MCRunState(const std::string &filename);
  virtual ~MCRunState(){}

  bool takeStep(VectorXd &curstate, RandAlgo &randEngine,
                std::normal_distribution<double> &dist);
  virtual bool stateIsValid(const VectorXd &state) = 0;
  virtual VectorXd findInitState() const = 0;

  void runSimulation();
  void finalize();
};

class EEConsRunState : public MCRunState {
  bool stateIsValid(const VectorXd &state) override;
  VectorXd findInitState() const override;

public:
  double getLo() const;
  double getHi() const;
  EEConsRunState(const MCRunSettings &settings, const std::string &filename)
      : MCRunState(settings, filename) {}
};

class VolConsRunState : public MCRunState {
  bool stateIsValid(const VectorXd &state) override;
  VectorXd findInitState() const override;

public:
  double getWallDist() const;
  VolConsRunState(const MCRunSettings &settings, const std::string &filename)
      : MCRunState(settings, filename) {}
};

class AngSpringRunState : public MCRunState {
  bool stateIsValid(const VectorXd &state) override;
  VectorXd findInitState() const override;

  public:
  double getSpringK() const;
  AngSpringRunState(const MCRunSettings &settings, const std::string &filename)
      : MCRunState(settings, filename) {}
};

inline double wrapAngle(double angle) {
  double twoPi = 2.0 * M_PI;
  return angle - twoPi * floor(angle / twoPi);
}