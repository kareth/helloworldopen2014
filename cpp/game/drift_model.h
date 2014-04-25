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

namespace game {

// We assume following drift model
//
// angle = x0 * previous_angle + x1 * previous_previous_angle + x2 * previous_velocity + x3
class DriftModel {
 public:
  DriftModel() {
    x_ = {1.9, -0.9, -0.00125, 0.00125 * sqrt(180000), 0.3};

    char filename[50];
    sprintf (filename, "bin/drift.csv");
    file_.open (filename);
    file_ << "angle,p_angle,velocity,radius,direction,next_angle" << std::endl;
  }

  ~DriftModel() {
    std::cout << "==== Drift Model ====" << std::endl;
    if (IsReady()) {
      for (int i = 0; i < x_.size(); i++)
        std::cout << "x" << i <<": " << x_[i] << " ";
      std::cout << std::endl;

      std::cout << "Model trained using: " << std::endl;
      for (int i = 0; i < model_.size(); ++i) {
        for (int j = 0; j < model_[i].size(); ++j) std::cout << std::setprecision(20) << model_[i][j] << " ";
        std::cout << std::endl;
      }
    }
    error_tracker_.Print();
    std::cout << std::endl;

    file_.close();
  }

  // next_angle = f(angle, previous_previous_angle, previous_velocity, radius, direction)
  void Record(double next_angle, double angle, double previous_angle, double velocity, double radius, double direction) {
    if (next_angle == 0) return;


    // TODO keep improving the model
    if (!IsReady() && direction != 0) {
      model_.push_back({
          angle,
          previous_angle,
          velocity * angle,
          -direction * velocity * velocity * sqrt(InvRadius(radius)),
          direction * velocity
      });
      b_.push_back(next_angle);
      if (model_.size() > 8) {
        Train();
      }
    }
    if (IsReady()) {
      error_tracker_.Add(Predict(angle, previous_angle, velocity, radius, direction), next_angle);
    }

    file_ << angle << "," << previous_angle << "," << velocity << "," << radius << "," << direction << "," << next_angle << std::endl;
  }

  // direction = {-1, 0, 1}
  double Predict(double angle, double previous_angle, double velocity, double radius, double direction) {
    // if (!IsReady()) return angle;
    return x_[0] * angle +
           x_[1] * previous_angle +
           x_[2] * velocity * angle +
           -direction * fmax(0, x_[3] * velocity * velocity * sqrt(InvRadius(radius)) - x_[4] * velocity);
  }

  bool IsReady() const {
    return ready_;
  }

 private:
  double InvRadius(double radius) {
    if (radius < 1e-5 && radius > -1e-5) return 0;
    return 1.0 / radius;
  }

  void Train() {
    ready_ = true;
    x_.clear();
    Simplex::Optimize(model_, b_, x_);
  }

  std::ofstream file_;

  bool ready_ = false;

  std::vector<vector<double>> model_;
  std::vector<double> b_;

  std::vector<double> x_;

  ErrorTracker error_tracker_;
};

}  // namespace game

#endif  // CPP_GAME_DRIFT_MODEL_H_
