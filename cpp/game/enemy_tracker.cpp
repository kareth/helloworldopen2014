#include "game/enemy_tracker.h"

using game::Position;
using game::Race;

namespace game {

EnemyTracker::EnemyTracker(game::CarTracker& car_tracker,
    const game::Race& race,
    const std::string& color,
    const Position& position)
  : car_tracker_(car_tracker), race_(race), color_(color), skip_(0),
    dead_(false) {
  state_ = CarState(position);
  //piece_speed_.resize(race_.track().pieces().size(), 0);
  //piece_speed_.resize(race_.track().pieces().size(), 0);
}

void EnemyTracker::RecordLapTime(int time) {
  // TODO what about that?:D
  lap_times_.push_back(time);
}

void EnemyTracker::RecordCrash() {
  skip_ = 30;
  dead_ = true;
}

void EnemyTracker::RecordPosition(const game::Position& position) {
  if (skip_ == 0) {
    dead_ = false;
  }
  if (skip_ >= 0 )
    skip_--;

  // if (skip_-- > 0)
  //  return;

  state_ = car_tracker_.CreateCarState(state_, position);

/*  int piece = state_.position().piece();

  piece_speed_[piece] =
    double(piece_speed_[piece] * piece_data_points_[piece] + state_.velocity()) /
    double(piece_data_points_[piece] + 1);

  piece_data_points_[piece]++;

  average_speed_ =
    double(average_speed_ * average_data_points_ + state_.velocity()) /
    double(average_data_points_ + 1);

  average_data_points_++;*/
}

/*Position EnemyTracker::PositionAfterTime(int time) {
  Position pos = state_.position();
  for (int i = 0; i < time; i++) {
    double velocity = piece_speed_[state_.position().piece()];
    if (velocity < 1e-9) // Not yet calculated
      velocity = average_speed_;

    pos = race_.track().PositionAfter(pos, velocity);
  }
  return pos;
}

int EnemyTracker::TimeToPosition(const Position& target) {
  Position pos = state_.position();

  int time = 0;
  int limit = 300;
  while (++time) {
    if (limit-- < 0) return 1000000;
    double velocity = piece_speed_[state_.position().piece()];
    if (velocity < 1e-9) // Not yet calculated
      velocity = average_speed_;

    pos = race_.track().PositionAfter(pos, velocity);

    // TODO unsafe...
    if ((pos.piece() == target.piece() && pos.piece_distance() > target.piece_distance())
        || (pos.piece() + 1) % race_.track().pieces().size() == target.piece())
      break;
  }
  return time;
}*/

}  // namespace game
