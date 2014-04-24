#include "game/lane_length_model.h"

namespace game {

double LaneLengthModel::Length(const Position& position) {
  const auto& piece = pieces_[position.piece()];

  if (piece.type() == PieceType::kStraight) {
    if (position.start_lane() == position.end_lane()) {
      return piece.length();
    }
    if (!piece.has_switch()) {
      std::cerr << "Changing lane on non switch piece?" << std::endl;
    }
    // TODO this is just approximation. Add learning.

    double width = fabs(lanes_[position.start_lane()].distance_from_center() - lanes_[position.end_lane()].distance_from_center());
    return 1.000783334 * std::sqrt(width * width + piece.length() * piece.length());
  }

  if (position.start_lane() == position.end_lane()) {
    double radius = LaneRadius(position.piece(), position.start_lane());
    return 2.0 * M_PI * radius * (fabs(piece.angle()) / 360.0);
  }

  if (!piece.has_switch()) {
    std::cerr << "Changing lane on non switch piece?" << std::endl;
  }

  // TODO this is just approximation. Add learning.
  double radius1 = LaneRadius(position.piece(), position.start_lane());
  double radius2 = LaneRadius(position.piece(), position.end_lane());
  return 1.05 * M_PI * radius1 * (fabs(piece.angle()) / 360.0) + M_PI * radius2 * (fabs(piece.angle()) / 360.0);
}

}  // namespace game
