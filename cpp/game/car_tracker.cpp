#include "game/car_tracker.h"

namespace game {

CarTracker::CarTracker(const Race* race) : race_(race) {
  stats_file_.open ("bin/stats.csv");
  stats_file_ << "piece_index,start_lane,end_lane,radius,in_piece_distance,angle,velocity,throttle" << std::endl;

  if (race_->track().id() == "germany" ||
      race_->track().id() == "keimola" ||
      race_->track().id() == "usa") {
    drift_model_[0].reset(new DriftModel(0));
    drift_model_[0]->AddModel({1.9, -0.9, -0.00125, 0});
    drift_model_[1].reset(new DriftModel(1));
    drift_model_[1]->AddModel({1.9, -0.9, -0.00125, 0.00125});
    drift_model_[-1].reset(new DriftModel(-1));
    drift_model_[-1]->AddModel({1.9, -0.9, -0.00125, -0.00125});
  }
}

CarState CarTracker::Predict(const CarState& state, const Command& command) {
  if (!IsReady()) {
    std::cerr << "Cannot predict on not ready model" << std::endl;
    return state;
  }

  double velocity = velocity_model_.Predict(state.velocity(), command.throttle());
  double piece_distance = state.position().piece_distance() + velocity;
  int lap = state.position().lap();
  int piece = state.position().piece();

  // Is it next piece?
  if (piece_distance > race_->track().LaneLength(piece, state.position().start_lane())) {
    piece_distance = piece_distance - race_->track().LaneLength(piece, state.position().start_lane());
    piece++;
    if (piece >= race_->track().pieces().size()) {
      piece = 0;
      lap++;
    }
  }

  double radius = race_->track().LaneRadius(state.position().piece(), state.position().start_lane());
  double angle = GetDriftModel(state.position())->Predict(
      state.position().angle(),
      state.previous_angle(),
      state.velocity(),
      radius);

  Position position;
  position.set_piece_distance(piece_distance);
  position.set_lap(lap);
  position.set_piece(piece);
  position.set_start_lane(state.position().start_lane());
  position.set_end_lane(state.position().end_lane());
  position.set_angle(angle);

  return CarState(position, velocity, state.position().angle());
}

bool CarTracker::IsReady() const {
  return velocity_model_.IsReady();
}

DriftModel* CarTracker::GetDriftModel(const Position& position) {
  auto& piece = race_->track().pieces().at(position.piece());
  int direction = sgn(piece.angle());
  if (drift_model_[direction] == nullptr) {
    drift_model_[direction].reset(new DriftModel(direction));
  }
  return drift_model_[direction].get();
}

void CarTracker::LogState() {
  const auto& position = positions_.back();

  stats_file_ << std::setprecision(std::numeric_limits<double>::digits10)
    << position.piece() << ","
    << position.start_lane() << ","
    << position.end_lane() << ","
    << race_->track().LaneRadius(position.piece(), position.start_lane()) << ","
    << position.piece_distance() << ","
    << angle_ << ","
    << velocity_ << ","
    << last_command_.throttle() << std::endl;
}

}  // namespace game
