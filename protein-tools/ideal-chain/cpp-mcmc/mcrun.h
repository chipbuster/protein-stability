#include <cstdint>
#include <random>

#include <Eigen/Core>
#include <Eigen/Dense>

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

  MCRunSettings(int numAngles, int64_t numSteps, int64_t skipsPerStep, double lo,
                double hi)
      : numAngles{numAngles}, numSteps{numSteps}, gaussWidth{(hi - lo)/(numAngles*numAngles)},
        skipPerStep{skipsPerStep}, lo{lo}, hi{hi} {}
};

struct MCRunState {
  MCRunSettings settings;
  int64_t curIndex;
  MatrixXf trace;
  int64_t accept;
  int64_t reject;

  MCRunState(const MCRunSettings &settings) : settings{settings} {
    this->trace = MatrixXf::Zero(settings.numAngles, settings.numSteps);
  }

  MCRunState(const MCRunSettings &settings, int lastStep, const MatrixXf &trace)
      : settings{settings} {
    this->curIndex = lastStep;
    this->trace = trace;
  }

  void serialize(const std::string &filename) const;
  bool takeStep(VectorXd &curstate, VectorXd &nextState,
                std::mt19937 &randEngine,
                std::normal_distribution<double> &dist);
  void runSimulation();
};

// MCRunState deserialize(const std::string &filename);