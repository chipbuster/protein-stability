#include <cmath>
#include <ctgmath>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <stdexcept>

#include <cassert>

#include "mcrun.h"

#define READSTREAM(stream, var)                                                \
  stream.read(reinterpret_cast<char *>(&var), sizeof(var))

#define WRITESTREAM(stream, var)                                               \
  stream.write(reinterpret_cast<const char *>(&var), sizeof var)

void MCRunState::serialize(const std::string &filename) const {
  std::ofstream ofs(filename, std::ios::binary);
  WRITESTREAM(ofs, this->settings.lo);
  WRITESTREAM(ofs, this->settings.hi);
  WRITESTREAM(ofs, this->settings.gaussWidth);
  WRITESTREAM(ofs, this->settings.numAngles);
  WRITESTREAM(ofs, this->settings.numSteps);
  WRITESTREAM(ofs, this->settings.skipPerStep);
  WRITESTREAM(ofs, this->curIndex);
  WRITESTREAM(ofs, this->accept);
  WRITESTREAM(ofs, this->reject);

  for (int j = 0; j < this->trace.cols(); ++j) {
    for (int i = 0; i < this->trace.rows(); ++i) {
      WRITESTREAM(ofs, this->trace(i, j));
    }
  }
}

void check_condition(bool condition, const std::string &msg) {
  if (!condition) {
    throw std::runtime_error(msg);
  }
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
    angle = wrapAngle(angle);
    endpt(0) += cos(angle);
    endpt(1) += sin(angle);
  }

  return endpt.norm();
}

bool MCRunState::takeStep(VectorXd &curState, VectorXd &nextState,
                          std::mt19937 &randEngine,
                          std::normal_distribution<double> &dist) {
  for (int i = 0; i < curState.rows(); ++i) {
    auto d = dist(randEngine);
    // std::cout << "Propose update on " << i << " of " << d << std::endl;
    curState(i) += d;
  }

  double eedist = computeEEDist(nextState);
  if (std::isnan(eedist)) {
    std::cerr << "ERROR: NaN value arose in calculation of E2E distance"
              << std::endl;
    throw std::runtime_error("Got NAaN while computing E2E");
  }
  if (eedist < this->settings.lo || eedist > this->settings.hi) {
    // Reject step
    nextState = curState;
    return false;
  } else {
    // We accept modifications, but we need to make sure to apply wrapping
    for (int i = 0; i < curState.rows(); ++i) {
      nextState(i) = wrapAngle(nextState(i));
    }
    return true;
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
  while (dist > r_hi || dist < r_lo) {
    // std::cout << "current guess " << guess << " has e2e of "<< dist << std::endl;
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
  std::random_device r;
  std::seed_seq seed2{r(), r(), r(), r(), r(), r(), r(), r()};
  std::mt19937 e2(seed2);
  std::normal_distribution<double> normal_dist(0.0, this->settings.gaussWidth);

  auto init = find_init_state(this->settings.numAngles, this->settings.lo,
                              this->settings.hi);

  int64_t accepted = 0;
  int64_t rejected = 0;

  VectorXd curState = init;
  assert(computeEEDist(init) >= this->settings.lo);
  assert(computeEEDist(init) <= this->settings.hi);
  VectorXd nextState = VectorXd::Zero(this->settings.numAngles);

  for (int step = 0; step < this->settings.numSteps; ++step) {
    for (int _unused = 0; _unused < this->settings.skipPerStep; ++_unused) {
      if (this->takeStep(curState, nextState, e2, normal_dist)) {
        auto dist = computeEEDist(nextState);
        assert(dist > this->settings.lo && dist < this->settings.hi);
        accepted += 1;
      } else {
        assert((curState - nextState).norm() < 1e-8);
        rejected += 1;
      }
      std::swap(curState, nextState);
    }
    this->trace.col(step) = curState.cast<float>();
    this->curIndex = step;
  }

  // TODO: Modify to allow out-of-core writing.

  std::cout << "Accepted " << accepted << " and rejected " << rejected
            << std::endl;
}