#include "VizHook.h"
#include "filereaders.h"
#include "polyscope/polyscope.h"

#include <cstdlib>
#include <iostream>
#include <string>
#include <thread>

static PhysicsHook *hook = NULL;

void toggleSimulation() {
  if (!hook)
    return;

  if (hook->isPaused()) {
    hook->run();
  } else
    hook->pause();
}

void resetSimulation() {
  if (!hook)
    return;

  hook->reset();
}

void drawGUICallback() {
  ImGui::PushItemWidth(100); // Make ui elements 100 pixels wide,
                             // instead of full width. Must have
                             // matching PopItemWidth() below.

  if (hook->showSimButtons() || true) {
    if (ImGui::CollapsingHeader("Start Simulation.",
                                ImGuiTreeNodeFlags_DefaultOpen)) {
      if (ImGui::Button("Run/Pause Sim")) {
        toggleSimulation();
      }
      if (ImGui::Button("Reset Sim")) {
        resetSimulation();
      }
    }
  }
  hook->drawGUI();
  ImGui::PopItemWidth();
  hook->render();
}

// void callback() {

//   static int numPoints = 2000;
//   static float param = 3.14;

//   ImGui::PushItemWidth(100);

//   // Curvature
//   if (ImGui::Button("add curvature")) {
//  //   addCurvatureScalar();
//   }

//   // Normals
//   if (ImGui::Button("add normals")) {
// //    computeNormals();
//   }

//   // Param
//   if (ImGui::Button("add parameterization")) {
//  //   computeParameterization();
//   }

//   // Geodesics
//   if (ImGui::Button("compute distance")) {
//  //   computeDistanceFrom();
//   }
//   ImGui::SameLine();
// //  ImGui::InputInt("source vertex", &iVertexSource);

//   ImGui::PopItemWidth();
// }

using std::cerr;
using std::cout;
using std::endl;

int main(int argc, char **argv) {
  if (argc < 3) {
    std::cout << "Usage: " << argv[0] << " <hdf5_file> <hdf5_datapath>"
              << std::endl;
    exit(1);
  }

  // Options
  polyscope::options::autocenterStructures = true;
  polyscope::view::windowWidth = 1024;
  polyscope::view::windowHeight = 1024;

  std::string hfile(argv[1]);
  std::string dpath(argv[2]);
  TraceType stype;
  if (dpath.find("chain") != std::string::npos) {
    stype = CHAIN;
  } else if (dpath.find("ring") != std::string::npos) {
    stype = RING;
  } else {
    std::cerr << "[WARN]: Could not determine trace type from hdf5_datapath.\n"
              << "Defaulting to CHAIN Type." << std::endl;
    stype = CHAIN;
  }

  // Initialize polyscope
  polyscope::init();

  hook = new VizHook(argv[1], argv[2], stype);
  hook->reset();

  polyscope::state::userCallback = drawGUICallback;

  polyscope::show();

  return 0;
}
