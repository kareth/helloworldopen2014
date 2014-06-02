#include "game/radius_model.h"

#include <cmath>

#include "game/position.h"
#include "game/track.h"
#include "game/piece.h"
#include "game/approximation.h"

namespace game {

RadiusModel::RadiusModel(const Track* track,
                         const LaneLengthModel* lane_length_model,
                         const SwitchRadiusParams& params)
    : track_(track), lane_length_model_(lane_length_model) {
  for (const auto& it : params.model) {
    double start_radius = std::get<0>(it.first);
    double end_radius = std::get<1>(it.first);
    double angle = std::get<2>(it.first);
    int percent = std::get<3>(it.first);
    double switch_radius = it.second;

    if (models_[std::make_tuple(start_radius, end_radius, angle)] == nullptr) {
      models_[std::make_tuple(start_radius, end_radius, angle)].reset(
          new SwitchRadiusModel(start_radius, end_radius, angle, lane_length_model_));
    }
    auto* model = models_[std::make_tuple(start_radius, end_radius, angle)].get();

    model->Add(percent, switch_radius);
  }
}

double RadiusModel::Radius(const Position& position) {
  const auto& piece = track_->pieces()[position.piece()];

  if (piece.type() == PieceType::kStraight)
    return 0.0;

  if (position.start_lane() == position.end_lane())
    return track_->LaneRadius(position.piece(), position.start_lane());

  return GetModel(position)->Radius(position.piece_distance());
}

void RadiusModel::Record(const Position& position, double radius) {
  if (radius < 1) return;
  if (std::isnan(radius)) return;
  if (position.start_lane() == position.end_lane()) return;

  auto* model = GetModel(position);

  model->Record(position.piece_distance(), radius);
}

double SwitchRadiusModel::Radius(double piece_distance) {
  MaybeUpdateLength();

  if (!HasLength()) {
    return 0.9 * min(start_radius_, end_radius_);
  }

  int percent = static_cast<int>(100.0 * piece_distance / length_);

  double radius = percent_to_radius_[percent];
  if (radius != -1) {
    return radius;
  }

  // TODO improve this
  return 0.9 * min(start_radius_, end_radius_);
}

void SwitchRadiusModel::Record(double piece_distance, double radius) {
  if (length_ == -1) {
    raw_data_.push_back({piece_distance, radius});
    return;
  }
  int percent = static_cast<int>(100.0 * piece_distance / length_);
  percent_to_radius_[percent] = radius;
}

void SwitchRadiusModel::MaybeUpdateLength() {
  if (HasLength())
    return;

  length_ = lane_length_model_->SwitchOnTurnLength(start_radius_, end_radius_, angle_);

  // Is length still unknown?
  if (!HasLength())
    return;

  for (const auto& p : raw_data_) {
    double piece_distance = p.first;
    double radius = p.second;

    int percent = static_cast<int>(100.0 * piece_distance / length_);
    percent_to_radius_[percent] = radius;
  }

  raw_data_.clear();
}

}  // namespace game
