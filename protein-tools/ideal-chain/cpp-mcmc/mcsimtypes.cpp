#include "mcrun.h"

///////////// Code for end-to-end distance contrained ////////////////////////

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
bool EEConsRunState::acceptProposal(const VectorXd &state) {
  double eedist = computeEEDist(state);
  if (std::isnan(eedist)) {
    std::cerr << "ERROR: NaN value arose in calculation of E2E distance"
              << std::endl;
    throw std::runtime_error("Got NaN while computing E2E");
  }

  double loe = this->getLo();
  double hie = this->getHi();

  return eedist >= loe && eedist <= hie;
}

Eigen::VectorXd EEConsRunState::findInitState() const {
  int64_t nangles = this->settings.numAngles;
  double r_lo = this->getLo();
  double r_hi = this->getHi();

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

double EEConsRunState::getLo() const { return this->settings.param1; }
double EEConsRunState::getHi() const { return this->settings.param2; }

////////////////// Code for volume contrained ////////////////////////

/* Note: volume-constrained is a little bit different from the other two
simulations in two important ways:

  - We treat the angles as 2-atom angles instead of the 3-atom angles we use
    in other simulations.
  - The first element of each state is not treated as an angle at all, but as
    the x-coordinate of the left endpoint.

These two facts combined mean that to get an N-atom chain, the user needs to
specify "numAngles" = N on the command line (unlike the other sims here, where
the user specifies numAngles = N-2; */

bool VolConsRunState::acceptProposal(const VectorXd &state) {
  // For volume-excluded chain, we check if any point's x-position lies in the
  // excluded region. Since our state is two-atom-angles, this is fairly
  // straightforward.

  double xPos = 0.0;
  constexpr double loe = 0.0;
  double hie = this->getWallDist();

  for (int i = 0; i < state.rows(); ++i) {
    xPos += cos(state(i));
    if (xPos > hie || xPos < loe) {
      return false;
    }
  }
  return true;
}

Eigen::VectorXd VolConsRunState::findInitState() const {
  Eigen::VectorXd guessVec =
      Eigen::VectorXd::Constant(this->settings.numAngles, M_PI / 2);
  for (int i = 1; i < this->settings.numAngles; i += 2) {
    guessVec(i) = -M_PI / 2;
  }

  return guessVec;
}

double VolConsRunState::getWallDist() const { return this->settings.param1; }

////////////////// Code for angular spring ////////////////////////

bool AngSpringRunState::acceptProposal(const VectorXd&) {
  double k = this->getSpringK();
  double curEnergy = 0.0;
  double prevEnergy = 0.0;

  for(int i = 0; i < curState.rows(); ++i){
    double displacement = curState(i) - M_PI;
    curEnergy += k * displacement * displacement;
  }

  for(int i = 0; i < prevState.rows(); ++i){
    double displacement = prevState(i) - M_PI;
    prevEnergy += k * displacement * displacement;
  }

  if(prevEnergy > curEnergy){
    return true;
  } else {
    // Compute acceptance ratios for MH-sampling
    double alpha = exp(prevEnergy - curEnergy);
    double u = this->unif_dist(this->e2);
    return u <= alpha;
  }
}

Eigen::VectorXd AngSpringRunState::findInitState() const {
  return Eigen::VectorXd::Constant(this->settings.numAngles, M_PI);
}

double AngSpringRunState::getSpringK() const { return this->settings.param1;}