#ifndef CPP_GAME_CAR_TRACKER_H_
#define CPP_GAME_CAR_TRACKER_H_

#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <string>

#include "game/car_predictor.h"
#include "game/crash_model.h"
#include "game/drift_model.h"
#include "game/error_tracker.h"
#include "game/gauss.h"
#include "game/position.h"
#include "game/race.h"
#include "game/simplex.h"
#include "game/velocity_model.h"

#define SQR(X) ((X)*(X))

using std::map;

namespace game {

template <typename T> int sgn(T val) {
    return (T(0) < val) - (val < T(0));
}

// Usage:
//
// CarTracker car_tracker(race);
//
// OnStep() {
//   car_tracker.Record(position);
//
//   // perform computation to choose best command
//   Command command = ...
//
//   car_tracker.RecordCommand(command);
// }
class CarTracker : public CarPredictor {
 public:
  CarTracker(const Race* race) : race_(race) {
    stats_file_.open ("bin/stats.csv");
    stats_file_ << "piece_index,radius,in_piece_distance,angle,velocity,throttle" << std::endl;

    if (race_->track().id() == "germany" || race_->track().id() == "keimola") {
      drift_model_[0].reset(new DriftModel(0));
      drift_model_[0]->AddModel({1.9, -0.9, -0.00125, 0});
      drift_model_[1].reset(new DriftModel(1));
      drift_model_[1]->AddModel({1.9, -0.9, -0.00125, 0.00125});
      drift_model_[-1].reset(new DriftModel(-1));
      drift_model_[-1]->AddModel({1.9, -0.9, -0.00125, -0.00125});
    }
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
        velocity = position.piece_distance() - prev_position.piece_distance() +
          race_->track().LaneLength(prev_position.piece(), prev_position.start_lane());
      }
    }

    // TODO do not update model after crash

    // For some reason first few entries are bogus. Do not train model on them.
    // Update Models
    crash_model_.Record(angle);
    velocity_model_.Record(velocity, velocity_, throttle_);
    // TODO last position
    auto pos = position;
    if (positions_.size() > 0) pos = positions_.back();

    double radius = race_->track().LaneRadius(pos.piece(), pos.start_lane());
    GetDriftModel(pos)->Record(angle, angle_, previous_angle_, velocity_, radius);

    // Update state
    previous_angle_ = angle_;
    angle_ = angle;
    velocity_ = velocity;
    positions_.push_back(position);

    LogState();
  }

  DriftModel* GetDriftModel(const Position& position) {
    auto& piece = race_->track().pieces().at(position.piece());
    int direction = sgn(piece.angle());
    if (drift_model_[direction] == nullptr) {
      drift_model_[direction].reset(new DriftModel(direction));
    }
    return drift_model_[direction].get();
  }

  // TODO change to RecordCommand
  void RecordThrottle(double throttle) {
    throttle_ = throttle;
  }

  void LogState() {
    const auto& position = positions_.back();

    stats_file_ << std::setprecision(std::numeric_limits<double>::digits10)
                << position.piece() << ","
                << race_->track().LaneRadius(position.piece(), position.start_lane()) << ","
                << position.piece_distance() << ","
                << angle_ << ","
                << velocity_ << ","
                << throttle_ << std::endl;
  }

  void RecordCarCrash() {
    crash_model_.RecordCarCrash(angle_);

    velocity_ = 0;
    angle_ = 0;

    stats_file_ << "CRASH" << std::endl;
  }

  double velocity() const { return velocity_; }

  double angle() const { return angle_; }

  CarState Predict(const CarState& state, const Command& command) {
    // TODO(tomek)
    return CarState();
  }

  Position Predict(const Position& position, const Position& previous_position, double throttle, bool change_lane) {
    if (!GetDriftModel(position)->IsReady() ||
        !velocity_model_.IsReady())
      return position;

    // TODO swapping lanes prolongs track :D
    // TODO no lane swapping management

    double radius = race_->track().LaneRadius(position.piece(), position.start_lane());
    double velocity = race_->track().Distance(position, previous_position);

    Position result;

    double angle = GetDriftModel(position)->Predict(
        position.angle(),
        previous_position.angle(),
        velocity,
        radius
        );

    result.set_angle(angle);

    double new_velocity = velocity_model_.Predict(velocity, throttle);
    double new_position = position.piece_distance() + new_velocity;

    int lap = position.lap();

    if (new_position > race_->track().LaneLength(position.piece(), position.start_lane())) {
      int piece = position.piece() + 1;
      if (piece >= race_->track().pieces().size()) {
        piece %= race_->track().pieces().size();
        lap++;
      }
      result.set_piece(piece);
      result.set_piece_distance(new_position - race_->track().LaneLength(position.piece(), position.start_lane()));
    } else {
      result.set_piece(position.piece());
      result.set_piece_distance(new_position);
    }

    result.set_lap(lap);

    // TODO(kareth)
    result.set_start_lane(position.start_lane());
    result.set_end_lane(position.end_lane());

    return result;
  }

  const vector<Position>& positions() { return positions_; }

  const CarState& current_state() {
    static CarState default_state;
    if (states_.size() > 0) {
    }
    return default_state;
  }

 private:
  std::ofstream stats_file_;

  double velocity_ = 0;
  double angle_ = 0;
  double previous_angle_ = 0;

  // Does not own the pointer.
  const Race* race_;

  CrashModel crash_model_;
  VelocityModel velocity_model_;
  // We need separate drift models for different pieces. We group them by
  // radius (straight pieces assume radius 0).
  // TODO fix drift model to make it independent on radius (sign)
  map<int, std::unique_ptr<DriftModel>> drift_model_;

  // TODO deprecated, use states instead
  vector<Position> positions_;
  vector<CarState> states_;
  double throttle_ = 0;
};

}  // namespace game

#endif  // CPP_GAME_CAR_TRACKER_H_
