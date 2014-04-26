#include "game/drift_model.h"

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
#include "gflags/gflags.h"

DECLARE_string(race_id);

namespace game {

void DriftModel::Record(double next_angle, double angle, double previous_angle, double velocity, double radius, double direction) {
  if (next_angle == 0) return;

  // TODO keep improving the model
  // TODO remove hardcoding of the next_angle. Maybe we should use crash model?
  if (!IsReady() && direction != 0 && next_angle > 5) {
    model_.push_back({
        angle,
        previous_angle,
        velocity * angle,
        -direction * velocity * velocity * sqrt(InvRadius(radius)),
        direction * velocity
        });
    b_.push_back(next_angle);
    // TODO
    if (model_.size() > 8) {
      Train();
    }
  }
  double predicted = 0;
  if (IsReady()) {
    predicted = Predict(angle, previous_angle, velocity, radius, direction);
    error_tracker_.Add(predicted, next_angle);
  }

  file_ << std::setprecision(20) << angle << "," << previous_angle << "," << velocity << "," << radius << "," << direction << "," << next_angle << "," << predicted - next_angle << std::endl;
}

double DriftModel::Predict(double angle, double previous_angle, double velocity, double radius, double direction) {
  // if (!IsReady()) return angle;
  return x_[0] * angle +
         x_[1] * previous_angle +
         x_[2] * velocity * angle +
         -direction * fmax(0, x_[3] * velocity * velocity * sqrt(InvRadius(radius)) - x_[4] * velocity);
}

double DriftModel::EstimateRadius(double next_angle, double angle, double previous_angle, double velocity, double direction) {
  double r =
    1 / pow((next_angle
          - x_[0] * angle +
          - x_[1] * previous_angle +
          - x_[2] * velocity * angle +
          - direction * x_[4] * velocity) / (-direction * x_[3] * velocity * velocity), 2);

  if (r > 0 && x_[3] * velocity * velocity * sqrt(1/r) - x_[4] * velocity < 1e-5) {
    return 0;
  }

  return r;
}

DriftModel::DriftModel() {
  x_ = {1.9, -0.9, -0.00125, 0.00125 * sqrt(180000), 0.3};

  file_.open("bin/" + FLAGS_race_id + "/drift.csv");
  file_ << "angle,p_angle,velocity,radius,direction,next_angle,error" << std::endl;
}

DriftModel::~DriftModel() {
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

}  // namespace game
