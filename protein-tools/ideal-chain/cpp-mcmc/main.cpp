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
        auto st = MCRunSettings(2, 1'000'000, 100, 0.2, 0.8);
        auto state = MCRunState(st);
        state.runSimulation();
}
