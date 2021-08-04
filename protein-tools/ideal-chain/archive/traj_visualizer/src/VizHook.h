#include "PhysicsHook.h"

#include "filereaders.h"
#include "polyscope/curve_network.h"
#include "polyscope/polyscope.h"

#include <Eigen/Core>
#include <unistd.h>

long microseconds_from_fps(long fps) {
  constexpr long microsec_per_sec = 1000000;
  return microsec_per_sec / fps;
}

class VizHook : public PhysicsHook {
public:
  VizHook(const char *filename, const char *datapath, TraceType tt)
      : PhysicsHook(), networkData(filename, datapath, tt), fps(15), frameId(0),
        maxFrames(networkData.length()) {}

  virtual void drawGUI() {
    ImGui::SliderInt("FPS Playback", &fps, 1, 60, "%d FPS");
    ImGui::SliderInt("Frame Number", &frameId, 1, this->maxFrames, "Frame #%d");
  }

  virtual void initSimulation() {
    maxFrames = networkData.length();
    this->frameId = 0;
    Q = networkData.getFrame(this->frameId);
    E = networkData.getTopo();

    polyscope::removeAllStructures();
    renderQ = Q;
    renderE = E;

    polyscope::registerCurveNetwork("cur state", renderQ, renderE);
  }

  virtual void updateRenderGeometry() {
    renderQ = Q;
    renderE = E;
  }

  // "Simulate" a step by reading the next step out of the file
  virtual bool simulateOneStep() {
    this->frameId++;
    Q = networkData.getFrameZeroMean(this->frameId);
    renderQ = Q;
    usleep(microseconds_from_fps(this->fps));
    return false;
  }

  virtual void renderRenderGeometry() {
    polyscope::getCurveNetwork()->updateNodePositions(renderQ);
    polyscope::requestRedraw();
  }

private:
  ENTrace networkData;

  int fps;
  int frameId;
  int maxFrames;

  Eigen::MatrixXd Q;
  Eigen::MatrixXi E;

  Eigen::MatrixXd renderQ;
  Eigen::MatrixXi renderE;
};