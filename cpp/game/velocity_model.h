#ifndef CPP_GAME_VELOCITY_MODEL_H_
#define CPP_GAME_VELOCITY_MODEL_H_

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <algorithm>

#include "game/gauss.h"
#include "game/error_tracker.h"

namespace game {

// We assume following velocity model
//
// velocity = x0 * previous_velocity + x1 * throttle
class VelocityModel {
 public:
  VelocityModel() {
    x_ = {0.2, 0.98};
  }
  ~VelocityModel() {
    std::cout << "==== Velocity Model ====" << std::endl;
    for (int i = 0; i < x_.size(); ++i)
      std::cout << "x" << i <<": " << x_[i] << " ";
    std::cout << std::endl;
    error_tracker_.Print();
    std::cout << std::endl;
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

  double PredictThrottle(double velocity_to_maintain) const {
    return (velocity_to_maintain - velocity_to_maintain * x_[0]) / x_[1];
  }

  bool IsReady() const {
    return ready_;
  }

 private:
  void Train() {
    GaussDouble(m_, b_, x_);
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
