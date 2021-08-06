#include <cmath>
#include <ctgmath>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <stdexcept>

#include <cassert>

#include "hdf5.h"
#include "mcrun.h"

////////////////////////////////////////////////////////////////////////////////

/* This simulation treats state as a vector of 3-atom angles (a.k.a. internal
angles). The rule is that the end-to-end distance must fall between lo and hi.
*/

/* These are the functions that need to be modified to change the monte carlo
   simulation. In principle, the definition of "stateIsValid" completely
   determines the simulation (in practice of course, other parts of the code
   may need to be touched ) */
inline double wrapAngle(double angle) {
  double twoPi = 2.0 * M_PI;
  return angle - twoPi * floor(angle / twoPi);
}

double computeEEDist(const VectorXd &angles) {
  double angle = 0.0;
  Eigen::Vector2d endpt;
  endpt << 1.0, 0.0;

  for (int i = 0; i < angles.rows(); ++i) {
    angle += angles(i) + M_PI;
    endpt(0) += cos(angle);
    endpt(1) += sin(angle);
  }

  return endpt.norm();
}

// Tests the proposed state to see if it is valid. Returns true if the proposed
// state is valid and false otherwise. Reminder: this function has access to the
// scratchBuf class instance variable if needed.
bool MCRunState::stateIsValid(const VectorXd &state) const {
  double eedist = computeEEDist(state);
  if (std::isnan(eedist)) {
    std::cerr << "ERROR: NaN value arose in calculation of E2E distance"
              << std::endl;
    throw std::runtime_error("Got NaN while computing E2E");
  }

  double loe = this->settings.lo;
  double hie = this->settings.hi;

  return eedist >= loe && eedist <= hie;
}

////////////////////////////////////////////////////////////////////////////////

constexpr int64_t MAX_BUF_SIZE = 1'000'000L;

void MCBuffer::recordState(const VectorXd &state, H5::DataSet &ds) {
  if (this->bufIndex >= static_cast<uint64_t>(this->buffer.cols())) {
    this->flush(ds);
    // Flush updates bufIndex so that buffer is effectively empty
  }
  this->buffer.col(this->bufIndex) = state;
  ++this->bufIndex;
}

void MCBuffer::flush(H5::DataSet &ds) {
  H5::DataSpace fspace = ds.getSpace();
  auto nA = static_cast<hsize_t>(this->buffer.rows());
  hsize_t slice_size[2] = {this->bufIndex, nA};
  hsize_t slice_loc[2] = {this->fileIndex, 0};
  fspace.selectHyperslab(H5S_SELECT_SET, slice_size, slice_loc);

  hsize_t mem_size[1] = {nA * bufIndex};
  H5::DataSpace mspace(1, mem_size);

  ds.write(this->buffer.data(), H5::PredType::NATIVE_DOUBLE, mspace, fspace);

  this->fileIndex += this->bufIndex;
  this->bufIndex = 0; // Wrote the entire buffer, so entire buffer can be reused
}

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
    : outBuf(settings.numAngles, std::min(1'000'000L, settings.numSteps)),
      settings{settings}, accept{0}, reject{0}, hdf5Filename{filename},
      e2(seed), normal_dist(0.0, settings.gaussWidth) {
  // Set internal buffer state
  this->curState = VectorXd::Zero(settings.numAngles);
  this->scratchBuf = VectorXd::Zero(settings.numAngles);

  hsize_t OUTBUF_NSAMP = std::min(1'000'000L, settings.numSteps);

  // Initialize HDF5 datasets
  this->outfile = H5::H5File(this->hdf5Filename.c_str(), H5F_ACC_EXCL);

  auto nA = static_cast<hsize_t>(this->settings.numAngles);
  auto nS = static_cast<hsize_t>(this->settings.numSteps);
  hsize_t maxdims[2] = {nS, nA};
  H5::DataSpace dataspace(2, maxdims);

  // Set chunking properties. Used to allow partial I/O on the receiving end
  H5::DSetCreatPropList cparms;
  hsize_t chunkDims[2] = {OUTBUF_NSAMP, nA};
  cparms.setChunk(2, chunkDims);

  H5::FloatType f32Ty(H5::PredType::NATIVE_FLOAT);
  this->ds = this->outfile.createDataSet(this->datasetName, f32Ty,
                                         dataspace);

  // Write the attributes we know about into the file. The final two
  // attributes (accept and reject) are not known until after the simulation
  // runs, so they can only be stored in finalize()
  this->settings.tagDataset(this->ds);
}


bool MCRunState::takeStep(VectorXd &curState, VectorXd &scratch,
                          RandAlgo &randEngine,
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
  double lo = 0.0;
  double guess = (hi + lo) / 2.0F;

  Eigen::VectorXd guessVec = Eigen::VectorXd::Constant(nangles, guess);
  double dist = sqrt(computeEEDist(guessVec));
  int guesscounter = 0;
  while (dist > r_hi || dist < r_lo) {
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

void MCRunState::runSimulation() {
  this->curState = find_init_state(this->settings.numAngles, this->settings.lo,
                                   this->settings.hi);
  assert(computeEEDist(this->curState) >= this->settings.lo);
  assert(computeEEDist(this->curState) <= this->settings.hi);

  for (int step = 0; step < this->settings.numSteps; ++step) {
    if (step % 10'000 == 0) {
      // std::cout << step << '\n';
    }
    for (int _unused = 0; _unused < this->settings.skipPerStep; ++_unused) {
      if (this->takeStep(this->curState, this->scratchBuf, this->e2,
                         normal_dist)) {
        this->accept += 1;
      } else {
        this->reject += 1;
      }
    }

    this->outBuf.recordState(curState, this->ds);
  }

  float pct = (100.0 * accept) / (accept + reject);
  std::cout << "Accepted " << pct << "% of all updates" << std::endl;

  this->finalize();
}

void MCRunState::finalize() {
  // We might have partial results stored in our buffer class. Make sure they
  // get written to file before we proceed.
  this->outBuf.flush(this->ds);

  H5::IntType i64Ty(H5::PredType::NATIVE_INT64);
  H5::DataSpace attSpace(H5S_SCALAR);

  H5::Attribute attAcc = this->ds.createAttribute("accepted", i64Ty, attSpace);
  attAcc.write(i64Ty, &this->accept);

  H5::Attribute attRej = this->ds.createAttribute("rejected", i64Ty, attSpace);
  attRej.write(i64Ty, &this->reject);

  this->outfile.flush(H5F_SCOPE_GLOBAL);
}