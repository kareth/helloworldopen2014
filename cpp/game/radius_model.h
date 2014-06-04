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
#include "game/physics_params.h"
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
      length_(lane_length_model->SwitchOnTurnLength(start_radius, end_radius, angle)) {
    percent_to_radius_.assign(100, -1);
    // For some strange reason, the first percent is always a straight.
    percent_to_radius_[0] = 0;
  }

  ~SwitchRadiusModel() {
  }

  // Returns 0 for straight line.
  double Radius(double piece_distance);

  void Record(double piece_distance, double radius);

  void MergeWith(SwitchRadiusParams* params) const {
    for (int i = 1; i < percent_to_radius_.size(); ++i) {
      if (percent_to_radius_[i] != -1) {
        params->model.insert(
            {std::make_tuple(start_radius_, end_radius_, angle_, i), percent_to_radius_[i]});
      }
    }
  }

  void Add(int percent, double radius) {
    if (percent_to_radius_[percent] != -1) {
      if (fabs(radius - percent_to_radius_[percent]) > 1e-5) {
        std::cerr << "Known radius and recorded one are different."
                  << " start_radius: " << start_radius_
                  << " end_radius: " << end_radius_
                  << " angle: " << angle_
                  << " percent: " << percent
                  << " known radius: " << percent_to_radius_[percent]
                  << " new radius: " << radius << std::endl;
      }
      return;
    }
    percent_to_radius_[percent] = radius;
  }

 private:
  double start_radius_;
  double end_radius_;
  double angle_;
  double length_;

  // If radius is -1, then it is unknown.
  vector<double> percent_to_radius_;

};

class RadiusModel {
 public:
  RadiusModel(const Track* track,
              const LaneLengthModel* lane_length_model,
              const SwitchRadiusParams& params);

  void Record(const Position& position, double radius);

  // Returns 0 for straight line.
  double Radius(const Position& position);

  SwitchRadiusParams CreateParams() {
    SwitchRadiusParams params;
    for (const auto& it : models_) {
      it.second->MergeWith(&params);
    }
    return params;
  }

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
