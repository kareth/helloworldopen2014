#include "game/enemy_tracker.h"

using game::Position;
using game::Race;

namespace game {

EnemyTracker::EnemyTracker(game::CarTracker& car_tracker,
    const game::Race& race,
    const std::string& color,
    const Position& position)
  : car_tracker_(car_tracker), race_(race), color_(color), skip_(kSkipTime),
    dead_(false), time_to_spawn_(0), dnf_(false), best_lap_(-1) {
  state_ = CarState(position);
  piece_speed_.resize(race_.track().pieces().size(), 0);
  piece_data_points_.resize(race_.track().pieces().size(), 0);
}

void EnemyTracker::RecordLapTime(int time) {
  // TODO what about that?:D
  lap_times_.push_back(time);
  if (best_lap_ == -1 || best_lap_ > time)
    best_lap_ = time;
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

void EnemyTracker::DNF() {
  dnf_ = true;
  FinishedRace();
}

void EnemyTracker::FinishedRace() {
  skip_ = 10000000;
  time_to_spawn_ = 10000000;
  dead_ = true;
}

void EnemyTracker::Resurrect() {
  if (!dnf_) {
    skip_ = kSkipTime;
    dead_ = false;
    time_to_spawn_ = 0;
  }
}

// Checks if time difference on that part of track is enough to overtake him
// Based on best laptime right now
bool EnemyTracker::CanOvertake(const EnemyTracker& noobek, int from, int to) {
  if (best_lap_ == -1 || noobek.best_lap() == -1)
    return true;

  // Cant overtake faster
  if (best_lap_ > noobek.best_lap())
    return false;

  // TODO hella inaccurate
  int pieces = from <= to ? to - from + 1 : to + race_.track().pieces().size() - from + 1;

  double percent = double(pieces) / double(race_.track().pieces().size());

  int ticks_difference = double(noobek.best_lap() - best_lap_) * percent;

  const double kCarLength = race_.cars().at(0).length();

  double distance = 0;
  for (int i = 0; i < ticks_difference; i++) {
    distance += piece_speed_[to];
    // TODO
    if (distance > 1.5 * kCarLength)
      return true;
  }

  return false;
}

void EnemyTracker::TurboStarted() {
  // TODO start turbo
}

void EnemyTracker::Spawned() {
  // TODO reset turbo
}

void EnemyTracker::NewTurbo(const Turbo& turbo) {
  state_.AddNewTurbo(turbo);
}

}  // namespace game
