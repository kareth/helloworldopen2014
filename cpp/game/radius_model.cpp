#include "game/radius_model.h"

#include <cmath>

#include "game/position.h"
#include "game/track.h"
#include "game/piece.h"
#include "game/simplex.h"

namespace game {

double RadiusModel::Radius(const Position& position) {
  const auto& piece = track_->pieces()[position.piece()];

  if (piece.type() == PieceType::kStraight)
    return 0.0;

  if (position.start_lane() == position.end_lane())
    return track_->LaneRadius(position.piece(), position.start_lane());

  const auto& model = models_[{position.piece(), position.start_lane()}];
  if (model.IsReady()) {
    return model.Radius(position.piece_distance());
  }

  // We try to make it safe
  return 0.9 *
      fmin(track_->LaneRadius(position.piece(), position.start_lane()),
           track_->LaneRadius(position.piece(), position.end_lane()));
}

void RadiusModel::Record(const Position& position, double radius) {
  if (radius < 1) return;
  if (position.start_lane() == position.end_lane()) return;

  auto& model = models_[{position.piece(), position.start_lane()}];

  if (model.IsReady()) {
    if (radius < model.Radius(position.piece_distance())) {
      std::cerr << "ERROR: We under estimate the radius" << std::endl;
      std::cerr << "Position: " << position.DebugString();
      std::cerr << "Radius: " << radius << std::endl;
      std::cerr << "Predicted: " << model.Radius(position.piece_distance()) << std::endl;
    }
  }

  model.Record(position.piece_distance(), radius);

  file_ << std::setprecision(20) << position.piece() << "," << position.start_lane() << "," << position.end_lane() << "," << position.piece_distance() << "," << radius << std::endl;
}

double SwitchRadiusModel::Radius(double piece_distance) const {
  return x_[0] * piece_distance * piece_distance +
         x_[1] * piece_distance +
         x_[2] - 1;
}

void SwitchRadiusModel::Record(double piece_distance, double radius) {
  model_.push_back({piece_distance * piece_distance, piece_distance, 1});
  b_.push_back(radius);

  if (model_.size() == 5) {
    ready_ = true;
    Simplex::Optimize(model_, b_, x_);
  }
}

}  // namespace game
