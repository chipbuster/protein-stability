#include "mcrun.h"

#include <cassert>
#include <chrono>
#include <iostream>
#include <sstream>
#include <vector>

using std::chrono::duration_cast;
using std::chrono::high_resolution_clock;
using std::chrono::seconds;

int main(int argc, char **argv) {
  std::cout << argc << argv[0] << '\n';
  auto st = MCRunSettings(7, 1'000'000, 100, 0.2, 0.8);
  MCRunState state(st, "/tmp/lol");
  state.runSimulation();
}
