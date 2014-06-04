#ifndef CPP_GAME_VELOCITY_MODEL_H_
#define CPP_GAME_VELOCITY_MODEL_H_

#include <cassert>
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <cmath>
#include <algorithm>

#include "game/physics_params.h"
#include "game/approximation.h"
#include "game/error_tracker.h"
#include "gflags/gflags.h"

DECLARE_bool(print_models);

namespace game {

// We assume following velocity model
//
// velocity = x0 * previous_velocity + x1 * throttle
class VelocityModel {
 public:
  VelocityModel(const VelocityModelParams& params) : error_tracker_("velocity") {
    assert(params.model.size() == 2);
    x_ = params.model;
  }
  ~VelocityModel() {
    if (FLAGS_print_models) {
      std::cout << "==== Velocity Model ====" << std::endl;
      for (int i = 0; i < x_.size(); ++i)
        std::cout << "x" << i <<": " << x_[i] << " ";
      std::cout << std::endl;
      error_tracker_.Print();
      std::cout << std::endl;
    }
  }

  // velocity = x * previous_velocity + y * previous_throttle
  void Record(double velocity, double previous_velocity, double previous_throttle) {
    if (velocity < 1e-5) return;
    if (previous_throttle < 1e-5) return;

    if (IsReady()) {
      error_tracker_.Add(velocity, Predict(previous_velocity, previous_throttle));
    }

    if (m_.size() < 2) {
      m_.push_back({previous_velocity, previous_throttle});
      b_.push_back(velocity);
    }

    if (m_.size() == 2 && !IsReady()) {
      Train();
    }
  }

  // Returns predicted new velocity based on velocity and applied throttle.
  double Predict(double velocity, double throttle) const {
    return x_[0] * velocity + x_[1] * throttle;
  }

  // Return distance when starting from initial velocity and using throttles
  double PredictDistance(double initial_velocity, const vector<double>& throttles) const {
    double distance = 0;
    double velocity = initial_velocity;
    for (int i = 0; i < throttles.size(); ++i) {
        velocity = Predict(velocity, throttles[i]);
        distance += velocity;
    }
    //TODO(Wojtek): We can improve performance by summing up geometric sequence
    return distance;
  }

  // Return distance when starting from initial velocity and using throttles how_many times
  // (For performance)
  double PredictDistance(double initial_velocity, int how_many, double throttle) const {
    /* The following is correct (magic!), but I am not sure if this quicker than the iteration
    double powx = std::pow(x_[0], how_many);
    double onex = 1 - x_[0];
    double left = x_[0] * (1 - powx) / onex * initial_velocity;
    double right = x_[1] * throttle * (how_many * onex - x_[0] * (1 - powx)) / (onex * onex);
    return left + right;
    */
      
    double distance = 0;
    double velocity = initial_velocity;
    for (int i = 0; i < how_many; ++i) {
        velocity = Predict(velocity, throttle);
        distance += velocity;
    }
    return distance;
  }

  double PredictThrottle(double velocity_to_maintain) const {
    return (velocity_to_maintain - velocity_to_maintain * x_[0]) / x_[1];
  }

  // Returns maximum throttle that will travel given distance (or smaller)
  // given that car drives with given velocity.
  //
  // If such throttle does not exist (we will always travel bigger distance,
  // false is returned.
  bool BoundaryThrottle(double velocity, double distance, double* throttle) {
    if (Predict(velocity, 1) <= distance) {
      *throttle = 1.0;
      return true;
    }

    if (Predict(velocity, 0) > distance) {
      return false;
    }

    *throttle = (distance - x_[0] * velocity) / x_[1];
    return true;
  }

  bool IsReady() const {
    return ready_;
  }

  VelocityModelParams CreateParams() const {
    VelocityModelParams params;
    params.model = x_;
    return params;
  }

 private:
  void Train() {
    Approximation(m_, b_, x_);
    ready_ = true;
  }

  bool ready_ = false;

  // Variables used to train the model
  // {previous_velocity, previous_throttle}
  std::vector<vector<double> > m_;
  // {velocity}
  std::vector<double> b_;

  // Model used for prediction.
  std::vector<double> x_;

  ErrorTracker error_tracker_;
};

}  // namespace game

#endif  // CPP_GAME_VELOCITY_MODEL_H_
