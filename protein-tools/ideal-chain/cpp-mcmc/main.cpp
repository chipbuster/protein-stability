#include"mcrun.h"

#include<iostream>
#include<cassert>
#include<sstream>
#include<chrono>
#include<vector>

using std::chrono::high_resolution_clock;
using std::chrono::duration_cast;
using std::chrono::seconds;

int main(int argc, char** argv){
        auto st = MCRunSettings(7, 1'000'000, 100, 0.2, 0.8);
        MCRunState state(st, "/tmp/lol");
        state.runSimulation();
}
