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

  H5::Attribute attP1 = dset.createAttribute("param1", f64Ty, attSpace);
  attP1.write(f64Ty, &this->param1);

  H5::Attribute attP2 = dset.createAttribute("param2", f64Ty, attSpace);
  attP2.write(f64Ty, &this->param2);

  H5::Attribute attP3 = dset.createAttribute("param3", f64Ty, attSpace);
  attP3.write(f64Ty, &this->param3);

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
  this->prevState = VectorXd::Zero(settings.numAngles);
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
  this->ds = this->outfile.createDataSet(this->datasetName, f32Ty, dataspace);

  // Write the attributes we know about into the file. The final two
  // attributes (accept and reject) are not known until after the simulation
  // runs, so they can only be stored in finalize()
  this->settings.tagDataset(this->ds);
}

bool MCRunState::takeStep() {
  this->prevState = this->curState;
  this->proposeUpdate(); // Places proposed update in curState

  if (acceptProposal(this->curState)) {
    return true;
  } else {
    std::swap(curState, prevState); // Reject step by restoring previous state
    return false;
  }
}

void MCRunState::proposeUpdate() {
  for (int i = 0; i < curState.rows(); ++i) {
    auto d = this->normal_dist(this->e2);
    curState(i) += d;
    curState(i) = wrapAngle(curState(i));
  }
}

void MCRunState::runSimulation() {
  this->curState = this->findInitState();

  for (int step = 0; step < this->settings.numSteps; ++step) {
    if (step % 10'000 == 0) {
      // std::cout << step << '\n';
    }
    for (int _unused = 0; _unused < this->settings.skipPerStep; ++_unused) {
      if (this->takeStep()) {
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