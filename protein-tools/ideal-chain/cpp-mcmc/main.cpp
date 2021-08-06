#include "mcrun.h"

#include <cassert>
#include <chrono>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

using std::chrono::duration;
using std::chrono::duration_cast;
using std::chrono::high_resolution_clock;
using std::chrono::seconds;

int main(int argc, char **argv) {
  if (argc < 3) {
    std::cout << "Usage: " << argv[0]
              << " <nangles> <npts> <nskip> <lo> <hi> <stepWidth> <outfile> "
              << std::endl;
    std::exit(1);
  }

  int natom = std::stoi(argv[1]);
  int nstep = std::stoi(argv[2]);
  int nskip = std::stoi(argv[3]);
  double lo_e = std::stod(argv[4]);
  double hi_e = std::stod(argv[5]);
  double gaussWidth = std::stod(argv[6]);

  std::string outf = argv[7];

  auto st = MCRunSettings(natom, nstep, nskip, lo_e, hi_e, gaussWidth);
  MCRunState state(st, outf);

  high_resolution_clock::time_point t1 = high_resolution_clock::now();
  state.runSimulation();
  high_resolution_clock::time_point t2 = high_resolution_clock::now();
  duration<double> time_span = duration_cast<duration<double>>(t2 - t1);
  std::cout << "Completed MCMC run in " << time_span.count() << " seconds" << '\n';
}
