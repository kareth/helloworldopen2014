#ifndef CPP_GAME_DRIFT_MODEL_H_
#define CPP_GAME_DRIFT_MODEL_H_

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <algorithm>

#include "game/error_tracker.h"
#include "game/gauss.h"
#include "game/position.h"
#include "game/race.h"
#include "game/simplex.h"
#include "game/approximation.h"

namespace game {

// We assume following drift model
//
// angle = x0 * previous_angle + x1 * previous_previous_angle + x2 * previous_velocity + x3
class DriftModel {
 public:
  DriftModel();

  ~DriftModel();

  // next_angle = f(angle, previous_previous_angle, previous_velocity, radius, direction)
  void Record(double next_angle, double angle, double previous_angle, double velocity, double radius, double direction);

  // direction = {-1, 0, 1}
  double Predict(double angle, double previous_angle, double velocity, double radius, double direction);

  double EstimateRadius(double next_angle, double angle, double previous_angle, double velocity, double direction);

  bool IsReady() const {
    return ready_;
  }

 private:
  double InvRadius(double radius) {
    if (radius < 1e-5 && radius > -1e-5) return 0;
    return 1.0 / radius;
  }

  void Train();

  void TrainWithStraight();

  void RemoveOutliers();

  void RemoveEmptyModels();

  double ComputeError() const;

  void TrimModels();

  std::ofstream file_;

  bool ready_ = false;

  std::vector<vector<double>> model_from_straight_;
  std::vector<double> b_from_straight_;

  std::vector<vector<double>> model_;
  std::vector<double> b_;

  std::vector<double> x_;

  ErrorTracker error_tracker_;
};

}  // namespace game

#endif  // CPP_GAME_DRIFT_MODEL_H_
