#include "game/enemy_tracker.h"

using game::Position;
using game::Race;

namespace game {

EnemyTracker::EnemyTracker(game::CarTracker& car_tracker,
    const game::Race& race,
    const std::string& color,
    const Position& position)
  : car_tracker_(car_tracker), race_(race), color_(color), skip_(kSkipTime),
    dead_(false), time_to_spawn_(0) {
  state_ = CarState(position);
  piece_speed_.resize(race_.track().pieces().size(), 0);
  piece_data_points_.resize(race_.track().pieces().size(), 0);
}

void EnemyTracker::RecordLapTime(int time) {
  // TODO what about that?:D
  lap_times_.push_back(time);
}

void EnemyTracker::RecordCrash() {
  skip_ = kRespawnTime + kSkipTime;
  time_to_spawn_ = kRespawnTime;
  dead_ = true;
}

void EnemyTracker::RecordPosition(const game::Position& position) {
  if (time_to_spawn_-- == 0)
    dead_ = false;
  if (skip_-- > 0)
    return;

  state_ = car_tracker_.CreateCarState(state_, position);

  int piece = state_.position().piece();

  piece_speed_[piece] =
    double(piece_speed_[piece] * piece_data_points_[piece] + state_.velocity()) /
    double(piece_data_points_[piece] + 1);

  piece_data_points_[piece]++;

  average_speed_ =
    double(average_speed_ * average_data_points_ + state_.velocity()) /
    double(average_data_points_ + 1);

  average_data_points_++;
}

Position EnemyTracker::PositionAfterTime(int time) {
  auto state = state_;
  for (int i = 0; i < min(time, 1000); i++) {
    double throttle = car_tracker_.velocity_model().PredictThrottle(
        Velocity(state.position().piece()));
    state = car_tracker_.Predict(state, Command(throttle));
  }
  return state.position();
}

double EnemyTracker::Velocity(int piece) {
  double velocity = piece_speed_[state_.position().piece()];
  if (velocity < 1e-9) // Not yet calculated
    velocity = average_speed_;
  return velocity;
}

int EnemyTracker::TimeToPosition(const Position& target) {
  auto state = state_;

  int time = 0;
  if (dead_) time = time_to_spawn_;
  for (int limit = 0; limit < 300; limit++, time++) {
    double throttle = car_tracker_.velocity_model().PredictThrottle(
        Velocity(state.position().piece()));
    state = car_tracker_.Predict(state, Command(throttle));

    if (race_.track().IsFirstInFront(state.position(), target))
      return time;
  }
  return 100000;
}

}  // namespace game
