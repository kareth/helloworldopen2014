#ifndef CPP_GAME_DRIFT_MODEL_H_
#define CPP_GAME_DRIFT_MODEL_H_

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <algorithm>

#include "game/physics_params.h"
#include "game/error_tracker.h"
#include "game/gauss.h"
#include "game/position.h"
#include "game/race.h"
#include "game/simplex.h"
#include "game/approximation.h"

namespace game {

// We assume following drift model
//
// next_angle = x0 * angle +
//              x1 * previous_angle +
//              x2 * angle * velocity +
//              -direction * max(0, x3 * velocity * velocity * sqrt(1 / radius) -
//                                  x4 * velocity)
class DriftModel {
 public:
  DriftModel(const DriftModelParams& params);

  ~DriftModel();

  // Called to train the model.
  //
  // TODO The following method assumes that it is called only with correct
  // points (without bumps). If it will be called with incorrect point during
  // learning (I don't think it happend but who knows), it will still learn correctly
  // after gathering more points (only last X points are used to train the model).
  void Record(double next_angle, double angle, double previous_angle, double velocity, double radius, double direction);

  // direction = {-1, 0, 1}
  //
  // Returns 'next_angle'. If IsReady() is false, it doesn't have to be correct
  // value but is at least some guess.
  double Predict(double angle, double previous_angle, double velocity, double radius, double direction);

  // Returns the radius that would cause given 'next_angle'.
  // This method is used to model switches on turns.
  double EstimateRadius(double next_angle, double angle, double previous_angle, double velocity, double direction);

  // True if we are pretty confident in our model (~1e-9 accuracy)
  // and we can start driving at our full potential.
  bool IsReady() const {
    return ready_;
  }

  std::vector<double> GetModel() const {
      return x_;
  }

 private:
  // Train the model 'x_' based on 'raw_points_'.
  void Train();

  // If true, we are pretty confident, that the model is ok.
  // But it is still possible to improve it (e.g. get more points
  // to get better accuracy) so we also use very ready below.
  bool ready_ = false;

  // We use it when we are very confident that model is correct.
  // Very confident means at least 30 points that prove our model is correct,
  // including at least 20 points from turn with centrifugal force greater than
  // friction (the part with 'max' is greater than 0).
  bool very_ready_ = false;

  // If true, the x_[2] has been approximated.
  bool straight_ready_ = false;

  // This is the actual model.
  std::vector<double> x_;

  // {angle, previous_angle, velocity, radius, direction, next_angle}
  std::vector<std::vector<double>> raw_points_turn_;
  std::vector<std::vector<double>> raw_points_straight_;

  ErrorTracker error_tracker_;
};

}  // namespace game

#endif  // CPP_GAME_DRIFT_MODEL_H_
