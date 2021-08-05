#include <cmath>
#include <ctgmath>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <stdexcept>

#include <cassert>

#include "hdf5.h"
#include "mcrun.h"

// Results in 4MB per nAangle (e.g. nAngles = 5 results in 20MB buffers)
constexpr int OUTBUF_NSAMP = 1'000'000;

void MCRunSettings::tagDataset(H5::DataSet &dset) const {
  H5::IntType i64Ty(H5::PredType::NATIVE_INT64);
  H5::FloatType f64Ty(H5::PredType::NATIVE_DOUBLE);
  H5::DataSpace attSpace(H5S_SCALAR);

  H5::Attribute attLo = dset.createAttribute("lo", f64Ty, attSpace);
  attLo.write(f64Ty, &this->lo);

  H5::Attribute attHi = dset.createAttribute("hi", f64Ty, attSpace);
  attHi.write(f64Ty, &this->hi);

  H5::Attribute attGW = dset.createAttribute("gaussWidth", f64Ty, attSpace);
  attGW.write(f64Ty, &this->gaussWidth);

  H5::Attribute attNA = dset.createAttribute("numAngles", i64Ty, attSpace);
  attNA.write(i64Ty, &this->numAngles);

  H5::Attribute attNS = dset.createAttribute("numSteps", i64Ty, attSpace);
  attNS.write(i64Ty, &this->numSteps);

  H5::Attribute attSkip = dset.createAttribute("skips", i64Ty, attSpace);
  attSkip.write(i64Ty, &this->skipPerStep);
}

MCRunState::MCRunState(const MCRunSettings &settings,
                       const std::string &filename)
    : settings{settings}, accept{0}, reject{0}, hdf5Filename{filename},
      e2(seed), normal_dist(0.0, settings.gaussWidth) {
  // Set internal buffer state
  this->curState = VectorXd::Zero(settings.numAngles);
  this->scratchBuf = VectorXd::Zero(settings.numAngles);

  // We have to set the chunk cache or we'll get terrible performance
  // (like 3x slower). Unfortunately, there is no C++ wrapper to do this--we'll
  // need to fall back to the C API to set the chunk cache. Details of args at
  // https://support.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-SetChunkCache
  // Note: in C++ API, this needs to be passed in at file creation, so we do
  // the math up here. This needs to be synced with the chunksizes below or
  // things may get screwy.
  size_t size_per_chunk = sizeof(float) * settings.numAngles * OUTBUF_NSAMP;
  size_t total_buf_nbytes = 10 * size_per_chunk; // Fit 10 chunks into buffer
  size_t num_hash_slots = 997; // A prime number about 100 times larger than 10
  double w0 = 0.75;
  H5::FileAccPropList plist = H5::FileAccPropList::DEFAULT;
  plist.setCache(100, num_hash_slots, total_buf_nbytes, w0);

  // Initialize HDF5 datasets
  this->outfile = H5::H5File(this->hdf5Filename.c_str(), H5F_ACC_EXCL,
                             H5::FileCreatPropList::DEFAULT, plist);

  auto nA = static_cast<hsize_t>(this->settings.numAngles);
  auto nS = static_cast<hsize_t>(this->settings.numSteps);
  hsize_t maxdims[2] = {nA, nS};
  H5::DataSpace dataspace(2, maxdims);

  // Set chunking properties. These are set so that each additional value of
  // numAngles creates about 0.4MB of data. Under this regime, N=5 is about 2MB
  // per chunk, N=20 is about 8MB per chunk.
  H5::DSetCreatPropList cparms;
  hsize_t chunkDims[2] = {nA, OUTBUF_NSAMP};
  cparms.setChunk(2, chunkDims);

  H5::FloatType f32Ty(H5::PredType::NATIVE_FLOAT);
  this->ds =
      this->outfile.createDataSet(this->datasetName, f32Ty, dataspace, cparms);

  // Finally, write the attributes we know about into the file. The final two
  // attributes (accept and reject) are not known until after the simulation
  // runs, so they can only be stored in finalize()
  this->settings.tagDataset(this->ds);
}

inline double wrapAngle(double angle) {
  double twoPi = 2.0 * M_PI;
  return angle - twoPi * floor(angle / twoPi);
}

/// Compute the end-to-end distance using a caller-provided scratch buffer to
/// compute cumulative angles.
double computeEEDist(const VectorXd &angles) {
  double angle = 0.0;
  Eigen::Vector2d endpt;
  endpt << 1.0, 0.0;

  for (int i = 0; i < angles.rows(); ++i) {
    angle += angles(i) + M_PI;
    endpt(0) += cos(angle);
    endpt(1) += sin(angle);
  }

  return endpt.squaredNorm();
}

// Tests the proposed state to see if it is valid. Returns true if the proposed
// state is valid and false otherwise.
bool MCRunState::stateIsValid(const VectorXd &state) const {
  double eedist = computeEEDist(state);
  if (std::isnan(eedist)) {
    std::cerr << "ERROR: NaN value arose in calculation of E2E distance"
              << std::endl;
    throw std::runtime_error("Got NAaN while computing E2E");
  }

  double losq = this->settings.lo * this->settings.lo;
  double hisq = this->settings.hi * this->settings.hi;

  return eedist >= losq && eedist <= hisq;
}

bool MCRunState::takeStep(VectorXd &curState, VectorXd &scratch,
                          xoroshiro128plus &randEngine,
                          std::normal_distribution<double> &dist) {
  scratch = curState;
  // In principle, we could take no arguments and just use the class state. In
  // practice, probably wiser to decouple simulation advancement from a class.
  // Instead, call this function using class members as arguments.
  for (int i = 0; i < curState.rows(); ++i) {
    auto d = dist(randEngine);
    // std::cout << "Propose update on " << i << " of " << d << std::endl;
    curState(i) += d;
  }

  if (stateIsValid(curState)) {
    // Accept the step, but apply wrapping to prevent overflows
    for (int i = 0; i < curState.rows(); ++i) {
      curState(i) = wrapAngle(curState(i));
    }
    return true;
  } else {
    std::swap(curState, scratch); // Reject step by restoring previous state
    return false;
  }
}

Eigen::VectorXd find_init_state(int64_t nangles, double r_lo, double r_hi) {
  double target_e2e = (r_lo + r_hi) / 2.0F;

  double hi = M_PI;
  double lo = M_PI - 2.0F * M_PI / (nangles + 1);
  double guess = (hi + lo) / 2.0F;

  Eigen::VectorXd guessVec = Eigen::VectorXd::Constant(nangles, guess);
  double dist = computeEEDist(guessVec);
  int guesscounter = 0;
  while (dist > r_hi*r_hi || dist < r_lo*r_lo) {
    if (dist > target_e2e) {
      hi = guess;
    } else {
      lo = guess;
    }
    guess = (hi + lo) / 2.0;

    ++guesscounter;
    guessVec = Eigen::VectorXd::Constant(nangles, guess);
    dist = computeEEDist(guessVec);

    if (guesscounter > 150) {
      throw std::runtime_error("Couldn't find valid initial state for chain");
    }
  }
  return guessVec;
}

// Records the given vector state into the `step`-th column of the HDF5 dataset
// Buffers internally within
void MCRunState::recordState(const VectorXd &state, int step) {
  H5::DataSpace fspace = this->ds.getSpace();
  auto nA = static_cast<hsize_t>(this->settings.numAngles);
  auto s = static_cast<hsize_t>(step);
  hsize_t slice_size[2] = {nA, 1};
  hsize_t slice_loc[2] = {0, s};
  fspace.selectHyperslab(H5S_SELECT_SET, slice_size, slice_loc);

  hsize_t mem_size[1] = {nA};
  H5::DataSpace mspace(1, mem_size);

  this->ds.write(state.data(), H5::PredType::NATIVE_DOUBLE, mspace, fspace);
}

void MCRunState::runSimulation() {
  this->curState = find_init_state(this->settings.numAngles, this->settings.lo,
                                   this->settings.hi);
  Eigen::VectorXf floatState = Eigen::VectorXf::Zero(this->curState.rows());

  assert(computeEEDist(this->curState) >= this->settings.lo);
  assert(computeEEDist(this->curState) <= this->settings.hi);

  for (int step = 0; step < this->settings.numSteps; ++step) {
    if (step % 10'000 == 0) {
      std::cout << step << '\n';
    }
    for (int _unused = 0; _unused < this->settings.skipPerStep; ++_unused) {
      if (this->takeStep(this->curState, this->scratchBuf, this->e2,
                         normal_dist)) {
        this->accept += 1;
      } else {
        this->reject += 1;
      }
    }

    // this->recordState(curState, step);
  }

  std::cout << "Accepted " << accept << " and rejected " << reject << std::endl;

  this->finalize();
}

void MCRunState::finalize() {
  H5::IntType i64Ty(H5::PredType::NATIVE_INT64);
  H5::DataSpace attSpace(H5S_SCALAR);

  H5::Attribute attAcc = this->ds.createAttribute("accepted", i64Ty, attSpace);
  attAcc.write(i64Ty, &this->accept);

  H5::Attribute attRej = this->ds.createAttribute("rejected", i64Ty, attSpace);
  attRej.write(i64Ty, &this->reject);

  this->outfile.flush(H5F_SCOPE_GLOBAL);
}