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
DECLARE_bool(print_models);
DEFINE_bool(log_drift_model, true, "");

namespace game {

namespace {
// Note: we assume that infinite radius (straight piece) has radius 0.
double InvRadius(double radius) {
  if (radius < 1e-5 && radius > -1e-5) return 0;
  return 1.0 / radius;
}
}  // anonymous namespace

double DriftModel::Predict(double angle, double previous_angle, double velocity, double radius, double direction) {
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
void DriftModel::Record(double next_angle, double angle, double previous_angle, double velocity, double radius, double direction) {
  // If next_angle is so small, we probably just started and we are not
  // drifting any more. Doesn't make sense to record such entries.
  if (fabs(next_angle) < 1e-5) return;

  if (direction != 0) {
    raw_points_turn_.push_back({angle, previous_angle, velocity, radius, direction, next_angle});
  } else {
    raw_points_straight_.push_back({angle, previous_angle, velocity, radius, direction, next_angle});
  }

  if (!very_ready_) {
    Train();
  }

  double predicted = 0;
  if (IsReady()) {
    predicted = Predict(angle, previous_angle, velocity, radius, direction);
    error_tracker_.Add(predicted, next_angle);
  }
}

void DriftModel::Train() {
  // We will solve A * x = b
  vector<vector<double> > A;
  vector<double> b;
  vector<double> x;

  if (!straight_ready_ && raw_points_straight_.size() >= 2) {
    for (int i = (int) raw_points_straight_.size() - 1; i >= 0; --i) {
      double angle = raw_points_straight_[i][0];
      double previous_angle = raw_points_straight_[i][1];
      double velocity = raw_points_straight_[i][2];
      double radius = raw_points_straight_[i][3];
      double direction = raw_points_straight_[i][4];
      double next_angle = raw_points_straight_[i][5];

      A.push_back({velocity * angle});
      b.push_back(next_angle - (x_[0] * angle + x_[1] * previous_angle));

      if (b.size() > 2) break;
    }

    double error = Approximation(A, b, x);
    if (error < 1e-9) {
      x_[2] = x[0];
      straight_ready_ = true;
      if (FLAGS_log_drift_model) std::cout << "INFO: Drift model, straight part ready (x[2] = " << x_[2] << ")" << std::endl;
    }

    A.clear();
    b.clear();
    x.clear();
  }

  for (int i = (int) raw_points_turn_.size() - 1; i >= 0; --i) {
    double angle = raw_points_turn_[i][0];
    double previous_angle = raw_points_turn_[i][1];
    double velocity = raw_points_turn_[i][2];
    double radius = raw_points_turn_[i][3];
    double direction = raw_points_turn_[i][4];
    double next_angle = raw_points_turn_[i][5];

    // Ignore points if velocity is too small or we are on straigh piece (the
    // first x0, x1, x2 are enough to compute next_angle). We need points were
    // we can assume that we can drop the "max" factor in the model.
    if (straight_ready_ && fabs(next_angle - (x_[0] * angle + x_[1] * previous_angle + x_[2] * angle * velocity)) < 1e-9) {
      continue;
    }

    A.push_back({
        velocity * angle,
        -direction * velocity * velocity * sqrt(InvRadius(radius)),
        direction * velocity});
    b.push_back(next_angle - (x_[0] * angle + x_[1] * previous_angle));

    if (!ready_ && b.size() == 4) break;

    if (b.size() >= 10) break;
  }

  // We use 4 points instead of 3 just to make sure we do not learn model
  // on incorrect ones.
  if (b.size() >= 4) {
    double error = Approximation(A, b, x);

    // If chosen points are incorrect, then do not learn model. Wait for better
    // points.
    if (error < 1e-9) {
      // Check if we were able to train x_[3] and x_[4]
      for (int i = 0; i < A.size(); ++i) {
        if (fabs(A[i][0] * x[0] - b[i]) < 1e-5) {
          if (FLAGS_log_drift_model) std::cout << "WARNING: We were driving too slow on some turns. Ignore this model and wait for better one." << std::endl;
          return;
        }
      }

      if (!ready_) std::cout << "INFO: Model ready" << std::endl;
      ready_ = true;
      straight_ready_ = true;
      x_ = {x_[0], x_[1], x[0], x[1], x[2]};
    } else {
      if (FLAGS_log_drift_model) std::cout << "WARNING: Learnt drift model has big error. Ignore this model and wait for better one." << std::endl;
      return;
    }

    if (b.size() >= 10) {
      std::cout << "INFO: Model very ready" << std::endl;
      very_ready_ = true;
    }
  }
}

DriftModel::DriftModel() : error_tracker_("drift") {
  x_ = {1.9, -0.9, -0.00125, 0.00125 * sqrt(180000), 0.3};
}

DriftModel::~DriftModel() {
  if (FLAGS_print_models) {
    std::cout << "==== Drift Model ====" << std::endl;
    if (IsReady()) {
      for (int i = 0; i < x_.size(); i++)
        std::cout << "x" << i <<": " << x_[i] << " ";
      std::cout << std::endl;
    }
    error_tracker_.Print();
    std::cout << std::endl;
  }
}

}  // namespace game
