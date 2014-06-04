#include <cstdlib>

#include "game/lane_length_model.h"
#include "game/geometry.h"
#include "gflags/gflags.h"

DECLARE_string(race_id);
DECLARE_bool(print_models);

namespace game {

LaneLengthModel::LaneLengthModel(const Track* track, const SwitchLengthParams& params) : track_(track) {
  switch_on_straight_length_ = params.switch_on_straight_length;

  for (const auto& piece : track->pieces()) {
    if (!piece.has_switch()) continue;
    if (piece.type() == PieceType::kStraight) continue;

    for (int i = 1; i < track->lanes().size(); ++i) {
      double start_radius = piece.radius() + track->lanes()[i - 1].distance_from_center();
      double end_radius = piece.radius() + track->lanes()[i].distance_from_center();
      double angle = fabs(piece.angle());

      SwitchOnTurnLength(start_radius, end_radius, angle);
      SwitchOnTurnLength(end_radius, start_radius, angle);
    }
  }
}

LaneLengthModel::~LaneLengthModel() {
  if (FLAGS_print_models) {
    std::cout << "==== Lane Length Model ====" << std::endl;
    std::cout << "Straight:" << std::endl;
    for (const auto& p : switch_on_straight_length_) {
      std::cout << "(" << p.first.first << "," << p.first.second << ") => " << p.second << std::endl;
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
  return SwitchOnTurnLength(radius1, radius2, fabs(piece.angle()));
}

void LaneLengthModel::Record(const Position& previous, const Position& current, double predicted_velocity) {
  if (previous.piece() == current.piece())
    return;
  if (previous.start_lane() == previous.end_lane())
    return;

  const auto& piece = track_->pieces()[previous.piece()];
  if (piece.type() != PieceType::kStraight)
    return;

  double switch_length = previous.piece_distance() + predicted_velocity - current.piece_distance();

  const double width = fabs(track_->lanes()[previous.start_lane()].distance_from_center() - track_->lanes()[previous.end_lane()].distance_from_center());
  auto it = switch_on_straight_length_.insert({{piece.length(), width}, switch_length});
  if (fabs(it.first->second - switch_length) > 1e-6) {
    std::cerr << std::setprecision(8) << "ERROR: Memorized switch length (" << it.first->second
              << ") is different that calculated one (" << switch_length << ") for straight switch "
              << piece.length() << " " << width << std::endl;
  }
  return;
}

double LaneLengthModel::SwitchOnTurnLength(double r1, double r2, double angle) const {
  auto it = switch_on_turn_length_.find(std::make_tuple(r1, r2, angle));
  if (it != switch_on_turn_length_.end()) {
    return it->second;
  }
  double length = QuadraticBezierCurve::CreateForSwitch(r1, r2, angle).Length();
  switch_on_turn_length_.insert({std::make_tuple(r1, r2, angle), length});
  return length;
}

SwitchLengthParams LaneLengthModel::CreateParams() {
  SwitchLengthParams params;
  params.switch_on_straight_length = switch_on_straight_length_;
  return params;
}

}  // namespace game
