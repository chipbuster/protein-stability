#include "mcrun.h"

#include <cassert>
#include <chrono>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#define REPORT_TIME(stmt)                                                      \
  high_resolution_clock::time_point t1 = high_resolution_clock::now();         \
  stmt;                                                                        \
  high_resolution_clock::time_point t2 = high_resolution_clock::now();         \
  duration<double> time_span = duration_cast<duration<double>>(t2 - t1);       \
  std::cout << "Completed MCMC run in " << time_span.count() << " seconds"     \
            << '\n';

using std::chrono::duration;
using std::chrono::duration_cast;
using std::chrono::high_resolution_clock;
using std::chrono::seconds;

int main(int argc, char **argv) {
  if (argc < 3) {
    std::cout << "Usage: " << argv[0]
              << " <nangles> <npts> <nskip> <stepWidth> <outfile> <SIMARGS>"
              << "\n SimArgs for EE are: <lo> <hi>"
              << "\n SimArgs for VOL are: <wallDist>"
              << "\n SimArgs for ANG are: <springStrength>" << std::endl;
    std::exit(1);
  }

  // Parameters that exist no matter the sim type
  int natom = std::stoi(argv[1]);
  int nstep = std::stoi(argv[2]);
  int nskip = std::stoi(argv[3]);
  double gaussWidth = std::stod(argv[4]);
  std::string outf = argv[5];

  std::string simType = std::getenv("MC_SIM_TYPE");
  if (simType == "EE") {
    double lo_e = std::stod(argv[6]);
    double hi_e = std::stod(argv[7]);

    Eigen::Vector3d params;
    params << lo_e, hi_e, 0.0;

    MCRunSettings settings(natom, nstep, nskip, params, gaussWidth);
    EEConsRunState simState(settings, outf);
    REPORT_TIME(simState.runSimulation());
  } else if (simType == "VOL") {
    double wallDist = std::stod(argv[6]);

    Eigen::Vector3d params;
    params << wallDist, 0.0, 0.0;

    MCRunSettings settings(natom, nstep, nskip, params, gaussWidth);
    VolConsRunState simState(settings, outf);

    REPORT_TIME(simState.runSimulation());
  } else if (simType == "ANG") {
    double springK = std::stod(argv[6]);

    Eigen::Vector3d params;
    params << springK, 0.0, 0.0;

    MCRunSettings settings(natom, nstep, nskip, params, gaussWidth);
    // AngSpringRunState simState(settings, outf);

    // REPORT_TIME(simState.runSimulation());
  } else {
    std::cerr << "The variable MC_SIM_TYPE was not specified. Please specify "
              << " one of EE, VOL, or ANG and rerun the sim" << std::endl;
  }
}
