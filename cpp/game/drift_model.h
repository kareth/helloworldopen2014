#ifndef CPP_GAME_DRIFT_MODEL_H_
#define CPP_GAME_DRIFT_MODEL_H_

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <algorithm>

#include "game/error_tracker.h"
#include "game/gauss.h"
#include "game/position.h"
#include "game/race.h"
#include "game/simplex.h"

namespace game {

// We assume following drift model
//
// angle = x0 * previous_angle + x1 * previous_previous_angle + x2 * previous_velocity + x3
class DriftModel {
 public:
  DriftModel() {
    x_ = {1.9, -0.9, -0.00125, -0.00125};

    char filename[50];
    sprintf (filename, "bin/drift.csv");
    file_.open (filename);
    file_ << "p_angle,p_p_angle,p_velocity,angle,radius,sgn" << std::endl;
  }

  ~DriftModel() {
    std::cout << "==== Drift Model ====" << std::endl;
    if (IsReady()) {
      for (int i = 0; i < x_.size(); i++)
        std::cout << "x" << i <<": " << x_[i] << " ";
      std::cout << std::endl;
    }
    error_tracker_.Print();
    std::cout << std::endl;

    file_.close();
  }

  // angle = f(previous_angle, previous_previous_angle, previous_velocity, radius, direction)
  void Record(double angle, double previous_angle, double previous_previous_angle, double previous_velocity, double radius, double direction) {
    if (angle == 0) return;

    file_ << previous_angle << "," << previous_previous_angle << "," << previous_velocity << "," << angle << "," << radius;
  }

  // direction = {-1, 0, 1}
  double Predict(double angle, double previous_angle, double velocity, double radius, double direction) {
    // if (!IsReady()) return angle;
    return x_[0] * angle +
           x_[1] * previous_angle +
           x_[2] * velocity * angle +
           x_[3] * direction * velocity * fmax(0, sqrt(double(180000) * velocity * velocity * InvRadius(radius)) - 240);
  }

  bool IsReady() {
    return ready_;
  }

 private:
  double InvRadius(double radius) {
    if (radius < 1e-5 && radius > -1e-5) return 0;
    return 1.0 / radius;
  }

  std::ofstream file_;

  bool ready_ = false;

  vector<double> x_;

  ErrorTracker error_tracker_;
};

}  // namespace game

#endif  // CPP_GAME_DRIFT_MODEL_H_
