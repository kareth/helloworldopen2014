#ifndef CPP_GAME_RADIUS_MODEL_H_
#define CPP_GAME_RADIUS_MODEL_H_

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <set>
#include <algorithm>

#include "game/error_tracker.h"
#include "game/lane_length_model.h"
#include "game/gauss.h"
#include "game/position.h"
#include "game/race.h"
#include "game/simplex.h"
#include "gflags/gflags.h"

DECLARE_string(race_id);
DECLARE_bool(print_models);

namespace game {

class SwitchRadiusModel {
 public:
  SwitchRadiusModel(double start_radius, double end_radius,
                    double angle, const LaneLengthModel* lane_length_model)
    : start_radius_(start_radius),
      end_radius_(end_radius),
      angle_(angle),
      lane_length_model_(lane_length_model) {
    percent_to_radius_.assign(100, -1);
    // For some strange reason, the first percent is always a straight.
    percent_to_radius_[0] = 0;
    // Will return -1 if unknown.
    length_ = lane_length_model_->SwitchOnTurnLength(start_radius_, end_radius_, angle_);
  }

  ~SwitchRadiusModel() {
  }

  // Returns 0 for straight line.
  double Radius(double piece_distance);

  void Record(double piece_distance, double radius);

 private:
  void MaybeUpdateLength();

  bool HasLength() {
    return length_ != -1;
  }

  double start_radius_;
  double end_radius_;
  double angle_;
  const LaneLengthModel* lane_length_model_;

  // (piece_distance, radius)
  // Used only to record points when length is not known yet.
  vector<std::pair<double, double>> raw_data_;

  // If radius is -1, then it is unknown.
  vector<double> percent_to_radius_;

  // -1 if unknown;
  double length_ = -1;
};

class RadiusModel {
 public:
  RadiusModel(const Track* track, const LaneLengthModel* lane_length_model)
      : track_(track), lane_length_model_(lane_length_model) {
  }

  void Record(const Position& position, double radius);

  // Returns 0 for straight line.
  double Radius(const Position& position);

 private:
  SwitchRadiusModel* GetModel(const Position& position) {
    const auto& piece = track_->pieces()[position.piece()];

    double start_radius = track_->LaneRadius(position.piece(), position.start_lane());
    double end_radius = track_->LaneRadius(position.piece(), position.end_lane());
    double angle = fabs(piece.angle());

    if (models_[std::make_tuple(start_radius, end_radius, angle)] == nullptr) {
      models_[std::make_tuple(start_radius, end_radius, angle)].reset(
          new SwitchRadiusModel(start_radius, end_radius, angle, lane_length_model_));
    }
    return models_[std::make_tuple(start_radius, end_radius, angle)].get();
  }

  // (start_radius, end_radius, angle) -> model
  std::map<std::tuple<double, double, double>, std::unique_ptr<SwitchRadiusModel>> models_;

  const Track* track_;
  const LaneLengthModel* lane_length_model_;
};

}  // namespace game

#endif  // CPP_GAME_RADIUS_MODEL_H_
