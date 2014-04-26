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

  const auto& model = *GetModel(position.piece(), position.start_lane());
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

  auto& model = *GetModel(position.piece(), position.start_lane());

  if (model.IsReady()) {
    double predicted = model.Radius(position.piece_distance());
    if (radius < model.Radius(position.piece_distance()) || (radius > 1e-5 && predicted < 1e-5)) {
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
  double radius = 
    x_[0] * piece_distance * piece_distance +
    x_[1] * piece_distance +
    x_[2] - 1;

  if (piece_distance < data_[0].first) {
    return fmin(radius, data_[0].second);
  }
  if (piece_distance > data_.back().first) {
    return fmin(radius, data_.back().second);
  }
  return radius;
}

void SwitchRadiusModel::Record(double piece_distance, double radius) {
  model_.push_back({piece_distance * piece_distance, piece_distance, 1});
  b_.push_back(radius);

  data_.push_back({piece_distance, radius});
  sort(data_.begin(), data_.end());

  if (EnoughData()) {
    ready_ = true;
    Simplex::Optimize(model_, b_, x_);
  }
}

namespace {
template <typename T> int sgn(T val) {
    return (T(0) < val) - (val < T(0));
}
}  // anonymous namespace

bool SwitchRadiusModel::EnoughData() {
  if (model_.size() < 5 || model_.size() > 15)
    return false;

  return true;
}

}  // namespace game
