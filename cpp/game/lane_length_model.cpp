#include "game/lane_length_model.h"

namespace game {

double LaneLengthModel::Length(const Position& position, bool* perfect) const {
  if (perfect) *perfect = true;
  const auto& piece = track_->pieces()[position.piece()];

  if (piece.type() == PieceType::kStraight) {
    if (position.start_lane() == position.end_lane()) {
      return piece.length();
    }
    if (!piece.has_switch()) {
      std::cerr << "Changing lane on non switch piece?" << std::endl;
    }

    const double width = fabs(track_->lanes()[position.start_lane()].distance_from_center() - track_->lanes()[position.end_lane()].distance_from_center());
    if (switch_on_straight_length_.count({piece.length(), width}) > 0) {
      return switch_on_straight_length_.at({piece.length(), width});
    }

    if (perfect) *perfect = false;
    return std::sqrt(width * width + piece.length() * piece.length());
  }

  if (position.start_lane() == position.end_lane()) {
    double radius = track_->LaneRadius(position.piece(), position.start_lane());
    return 2.0 * M_PI * radius * (fabs(piece.angle()) / 360.0);
  }

  if (!piece.has_switch()) {
    std::cerr << "Changing lane on non switch piece?" << std::endl;
  }

  double radius1 = track_->LaneRadius(position.piece(), position.start_lane());
  double radius2 = track_->LaneRadius(position.piece(), position.end_lane());

  if (switch_on_turn_length_.count({radius1, radius2}) > 0) {
    return switch_on_turn_length_.at({radius1, radius2});
  }
  // The opposite switch is much better predictor if available
  if (switch_on_turn_length_.count({radius2, radius1}) > 0) {
    if (perfect) *perfect = false;
    return switch_on_turn_length_.at({radius1, radius2});
  }
  return M_PI * radius1 * (fabs(piece.angle()) / 360.0) + M_PI * radius2 * (fabs(piece.angle()) / 360.0);
}

void LaneLengthModel::Record(const Position& previous, const Position& current, double predicted_velocity) {
  if (previous.piece() == current.piece())
    return;
  if (previous.start_lane() == previous.end_lane())
    return;

  const auto& piece = track_->pieces()[previous.piece()];

  double length = previous.piece_distance() + predicted_velocity - current.piece_distance();

  if (piece.type() == PieceType::kStraight) {
    const double width = fabs(track_->lanes()[previous.start_lane()].distance_from_center() - track_->lanes()[previous.end_lane()].distance_from_center());
    switch_on_straight_length_[{piece.length(), width}] = length;
    return;
  }

  double radius1 = track_->LaneRadius(previous.piece(), previous.start_lane());
  double radius2 = track_->LaneRadius(previous.piece(), previous.end_lane());
  switch_on_turn_length_[{radius1, radius2}] = length;
}

}  // namespace game
