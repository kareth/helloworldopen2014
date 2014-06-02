#include <cstdlib>

#include "game/lane_length_model.h"
#include "gflags/gflags.h"

DECLARE_string(race_id);
DECLARE_bool(print_models);

namespace game {

LaneLengthModel::LaneLengthModel(const Track* track, const SwitchLengthParams& params) : track_(track) {
  params.LogMissingData(*track);
  switch_on_straight_length_ = params.switch_on_straight_length;
  switch_on_turn_length_ = params.switch_on_turn_length;
}

LaneLengthModel::~LaneLengthModel() {
  if (FLAGS_print_models) {
    std::cout << "==== Lane Length Model ====" << std::endl;
    std::cout << "Straight:" << std::endl;
    for (const auto& p : switch_on_straight_length_) {
      std::cout << "(" << p.first.first << "," << p.first.second << ") => " << p.second << std::endl;
    }
    std::cout << "Turn:" << std::endl;
    for (const auto& p : switch_on_turn_length_) {
      std::cout << "(" << std::get<0>(p.first) << "," << std::get<1>(p.first) << "," << std::get<2>(p.first) << ") => " << p.second << std::endl;
    }
  }
}

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

  if (switch_on_turn_length_.count(std::make_tuple(radius1, radius2, fabs(piece.angle()))) > 0) {
    return switch_on_turn_length_.at(std::make_tuple(radius1, radius2, fabs(piece.angle())));
  }
  if (perfect) *perfect = false;
  // The opposite switch is much better predictor if available
  if (switch_on_turn_length_.count(std::make_tuple(radius2, radius1, fabs(piece.angle()))) > 0) {
    return switch_on_turn_length_.at(std::make_tuple(radius2, radius1, fabs(piece.angle())));
  }
  return M_PI * radius1 * (fabs(piece.angle()) / 360.0) + M_PI * radius2 * (fabs(piece.angle()) / 360.0);
}

void LaneLengthModel::Record(const Position& previous, const Position& current, double predicted_velocity) {
  if (previous.piece() == current.piece())
    return;
  if (previous.start_lane() == previous.end_lane())
    return;

  const auto& piece = track_->pieces()[previous.piece()];

  double switch_length = previous.piece_distance() + predicted_velocity - current.piece_distance();

  if (piece.type() == PieceType::kStraight) {
    const double width = fabs(track_->lanes()[previous.start_lane()].distance_from_center() - track_->lanes()[previous.end_lane()].distance_from_center());
    auto it = switch_on_straight_length_.insert({{piece.length(), width}, switch_length});
    if (fabs(it.first->second - switch_length) > 1e-6) {
      std::cerr << std::setprecision(8) << "ERROR: Memorized switch length (" << it.first->second
                << ") is different that calculated one (" << switch_length << ") for straight switch "
                << piece.length() << " " << width << std::endl;
    }
    return;
  }

  double radius1 = track_->LaneRadius(previous.piece(), previous.start_lane());
  double radius2 = track_->LaneRadius(previous.piece(), previous.end_lane());
  auto it = switch_on_turn_length_.insert(
      {std::make_tuple(radius1, radius2, fabs(piece.angle())), switch_length});

  if (fabs(it.first->second - switch_length) > 1e-6) {
    std::cerr << std::setprecision(8) << "ERROR: Memorized switch length (" << it.first->second
              << ") is different that calculated one (" << switch_length << ") for turn switch "
              << radius1 << " " << radius2 << std::endl;
  }
}

double LaneLengthModel::SwitchOnTurnLength(
    double start_radius,
    double end_radius,
    double angle) const {
  auto it = switch_on_turn_length_.find(std::make_tuple(start_radius, end_radius, angle));
  if (it == switch_on_turn_length_.end()) {
    return -1;
  }
  return it->second;
}

SwitchLengthParams LaneLengthModel::CreateParams() {
  SwitchLengthParams params;
  params.switch_on_turn_length = switch_on_turn_length_;
  params.switch_on_straight_length = switch_on_straight_length_;
  return params;
}

}  // namespace game
