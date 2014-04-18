#ifndef CPP_GAME_CAR_TRACKER_H_
#define CPP_GAME_CAR_TRACKER_H_

#include <iostream>
#include <fstream>
#include <string>
#include <map>

#include "game/race.h"
#include "game/position.h"

namespace game {

class ErrorTracker {
 public:
  void Add(double error) {
    error_max_ = fmax(error, error_max_);
    error_sum_ += error;
    error_count_++;
  }

  void Add(double predicted, double actual) {
    Add(fabs(predicted - actual));
  }

  void Print() {
    std::cout << "Error Avg: " << (error_sum_ / error_count_) << " Max: " << error_max_ << std::endl;
  }

 private:
  double error_max_ = 0.0;
  double error_sum_ = 0.0;
  int error_count_ = 0;
};

class DriftModel {
};

// We assume following velocity model
//
// velocity = x * previous_velocity + y * throttle
class VelocityModel {
 public:
  ~VelocityModel() {
    // Dump statistics + errors
    std::cout << "==== Velocity Model ====" << std::endl;
    std::cout << "x: " << x_ << " y: " << y_ << std::endl;
    error_tracker_.Print();
  }

  // velocity = x * previous_velocity + y * previous_throttle
  void Record(double velocity, double previous_velocity, double previous_throttle) {
    if (velocity == 0) return;

    // Validate the model if ready
    if (IsReady()) {
      error_tracker_.Add(velocity, Predict(previous_velocity, previous_throttle));
    }

    model_.push_back({velocity, previous_velocity, previous_throttle});

    if (model_.size() == 2) {
      Train();
    }
  }

  // Returns predicted new velocity based on velocity and applied throttle.
  double Predict(double velocity, double throttle) {
    return x_ * velocity + y_ * throttle;
  }

  bool IsReady() {
    return ready_;
  }

 private:
  void Train() {
    // TODO(tomek) use gauss.
    y_ = model_[0][0] / model_[0][2];
    x_ = (model_[1][0] - y_ * model_[1][2]) / model_[1][1];
    ready_ = true;
  }

  // TODO remove it
  double x_ = 1;
  double y_ = 0;

  // {velocity, previous_velocity, previous_throttle}
  bool ready_ = false;
  vector<vector<double> > model_;

  ErrorTracker error_tracker_;
};


class CrashModel {
 public:
  ~CrashModel() {
    std::cout << "==== Crash Model ====" << std::endl;
    std::cout << "threshold: " << angle_threshold_ << std::endl;
    std::cout << (ready_ ? "with crash" : "without crash") << std::endl;
  }

  void RecordCarCrash(double angle) {
    ready_ = true;
    angle_threshold_ = fmax(angle_threshold_, angle);
  }

  void Record(double angle) {
    angle_threshold_ = fmax(angle_threshold_, angle);
  }

  // WillCrash?
  bool Predict(double angle) const {
    if (ready_) return false;
    return angle < angle_threshold_;
  }

  bool IsReady() const {
    return ready_;
  }

 private:

  // The angle that will not cause crash.
  // HACK(tomek) Hardcoded for finland track. Change to false and 0.
  bool ready_ = false;
  double angle_threshold_ = 0.0;
};

class CarTracker {
 public:
  CarTracker(const Race* race) : race_(race) {
    stats_file_.open ("bin/stats.csv");
    stats_file_ << "piece_index,in_piece_distance,angle,velocity" << std::endl;
  }

  ~CarTracker() {
    stats_file_.close();
  }

  void Record(const Position& position) {
    // Note: all variables with _ are old e.g.:
    // velocity_ - previous velocity
    // velocity  - new velocity

    // Calculate new values for state - velocity, angle, etc.
    double angle = position.angle();
    double velocity = velocity_;
    if (positions_.size() > 0) {
      const auto& prev_position = positions_.back();

      if (prev_position.piece() == position.piece()) {
        velocity = position.piece_distance() - prev_position.piece_distance();
      } else {
        velocity = velocity_model_.Predict(velocity_, throttle_);
      }
    }

    // TODO do not update model after crash

    // For some reason first few entries are bogus. Do not train model on them.
    // Update Models
    crash_model_.Record(angle);
    velocity_model_.Record(velocity, velocity_, throttle_);

    // Update state
    angle_ = angle;
    velocity_ = velocity;
    positions_.push_back(position);

    LogState();
  }

  void RecordThrottle(double throttle) {
    throttle_ = throttle;
  }

  void LogState() {
    const auto& position = positions_.back();

    stats_file_ << position.piece() << ","
                << position.piece_distance() << ","
                << angle_ << ","
                << velocity_ << std::endl;
  }

  void RecordCarCrash() {
    crash_model_.RecordCarCrash(angle_);

    velocity_ = 0;
    angle_ = 0;

    stats_file_ << "CRASH" << std::endl;
  }

  double velocity() const { return velocity_; }

  double angle() const { return angle_; }

 private:
  std::ofstream stats_file_;

  double velocity_ = 0;
  double angle_ = 0;

  const Race* race_;

  CrashModel crash_model_;
  VelocityModel velocity_model_;

  vector<Position> positions_;
  double throttle_ = 0;
};


}  // namespace game

#endif  // CPP_GAME_CAR_TRACKER_H_
