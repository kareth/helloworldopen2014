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
DEFINE_bool(debug_drift, false, "");

namespace game {

void DriftModel::Record(double next_angle, double angle, double previous_angle, double velocity, double radius, double direction) {
  if (fabs(next_angle) < 1e-5) return;

  if (direction == 0) {
    model_from_straight_.push_back({
        angle,
        previous_angle,
        velocity * angle,
        });
    b_from_straight_.push_back(next_angle);
  }

  if (direction != 0) {
    model_.push_back({
        angle,
        previous_angle,
        velocity * angle,
        -direction * velocity * velocity * sqrt(InvRadius(radius)),
        direction * velocity
        });
    b_.push_back(next_angle);
    // TODO
  }

  if (model_from_straight_.size() > 5 && model_from_straight_.size() < 20) {
    TrainWithStraight();
  } else if (model_.size() > 8 && model_.size() < 20) {
    Train();
  } else if (!IsReady() && model_.size() > 20) {
    std::cout << "ERROR We got a lot of samples, but model is not ready yet :(" << std::endl;
    TrainWithStraight();
  }


  double predicted = 0;
  if (IsReady()) {
    predicted = Predict(angle, previous_angle, velocity, radius, direction);
    error_tracker_.Add(predicted, next_angle);
  }

  file_ << std::setprecision(20) << angle << "," << previous_angle << "," << velocity << "," << radius << "," << direction << "," << next_angle << "," << predicted - next_angle << std::endl;
}

double DriftModel::ComputeError() const {
  double error = 0.0;
  for (int i = 0; i < model_.size(); ++i) {
    double predicted = 0.0;
    for (int j = 0; j < x_.size(); ++j) {
      predicted += model_[i][j] * x_[j];
    }
    error += (predicted - b_[i]) * (predicted - b_[i]);
  }
  return error;
}

void DriftModel::Train() {

  vector<double> old_x = x_;

  Approximation(model_, b_, x_);

  if (ComputeError() / model_.size() > 1e-9) {
    x_ = old_x;
    ready_ = false;
  } else {
    ready_ = true;
  }
}

void DriftModel::TrainWithStraight() {
  vector<double> old_x = x_;
  vector<double> xs;

  Approximation(model_from_straight_, b_from_straight_, xs);

  if (FLAGS_debug_drift) {
    std::cout << "Learnt from straight:" << std::endl;
    for (int i = 0 ; i < xs.size(); ++i) std::cout << xs[i] << " ";
    std::cout << std::endl;
  }

  for (int i = 0; i < model_.size(); ++i) {
    double predicted = 0.0;
    for (int j = 0; j < xs.size(); ++j) {
      predicted += model_[i][j] * xs[j];
    }
    if (fabs(predicted - b_[i]) < 1e-9) {
      if (FLAGS_debug_drift) {
        std::cout << "REMOVED unnecessary row from model" << std::endl;
        for (int j = 0 ; j < model_[i].size(); ++j) std::cout << std::setprecision(20) << model_[i][j] << " ";
        std::cout << std::endl;
      }

      for (int j = 0; j < model_[i].size(); ++j) model_[i][j] = 0.0;
      b_[i] = 0;
    }
  }
  RemoveEmptyModels();

  TrimModels();
  Approximation(model_, b_, x_);

  // Remove outliers
  int with_errors = 0;
  for (int i = 0; i < model_.size(); ++i) {
    double predicted = 0.0;
    for (int j = 0; j < x_.size(); ++j) {
      predicted += model_[i][j] * x_[j];
    }
    if (fabs(predicted - b_[i]) > 1e-5) {
      with_errors++;
    }
  }

  if (with_errors < 0.1 * model_.size() && with_errors > 0) {
    std::cout << "FOUND OUTLIERS " << with_errors << " for " << model_.size() << std::endl;
    RemoveOutliers();
  }
  Approximation(model_, b_, x_);

  if (FLAGS_debug_drift) {
    std::cout << "Full model: " << std::endl;
    for (int i = 0 ; i < x_.size(); ++i) std::cout << x_[i] << " ";
    std::cout << std::endl;
  }

  if (ComputeError() / model_.size() > 1e-9) {
    x_ = old_x;
  } else {
    ready_ = true;
  }
}

template <class T>
static void TrimVector(int after_size, vector<T>* x) {
  if (x->size() <= after_size) return;
  x->erase(x->begin(), x->begin() + (x->size() - after_size));
}

void DriftModel::TrimModels() {
  TrimVector(50, &model_);
  TrimVector(50, &b_);
  TrimVector(50, &model_from_straight_);
  TrimVector(50, &b_from_straight_);
}

void DriftModel::RemoveOutliers() {
  int with_errors = 0;
  for (int i = 0; i < model_.size(); ++i) {
    double predicted = 0.0;
    for (int j = 0; j < x_.size(); ++j) {
      predicted += model_[i][j] * x_[j];
    }
    if (fabs(predicted - b_[i]) > 1e-5) {
      for (int j = 0; j < x_.size(); ++j) model_[i][j] = 0.0;
      b_[i] = 0;
    }
  }
  RemoveEmptyModels();
}

void DriftModel::RemoveEmptyModels() {
  for (int i = 0; i < model_.size(); ++i) {
    bool zero = true;
    for (int j = 0 ; j < model_[i].size(); ++j) if (model_[i][j] != 0) { zero = false; break; }
    if (zero) {
      model_.erase(model_.begin() + i);
      b_.erase(b_.begin() + i);
      i--;
    }
  }
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

DriftModel::DriftModel() : error_tracker_("drift") {
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

    if (FLAGS_debug_drift) {
      std::cout << "Model trained using: " << std::endl;
      for (int i = 0; i < model_.size(); ++i) {
        for (int j = 0; j < model_[i].size(); ++j) std::cout << std::setprecision(20) << model_[i][j] << " ";
        std::cout << std::endl;
      }
    }
  }
  error_tracker_.Print();
  std::cout << std::endl;

  file_.close();
}

}  // namespace game
