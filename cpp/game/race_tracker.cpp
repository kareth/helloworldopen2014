#include "game/race_tracker.h"

using game::Position;
using game::Race;

namespace game {

RaceTracker::RaceTracker(game::CarTracker& car_tracker,
          const game::Race& race, const std::string& color)
  : car_tracker_(car_tracker), race_(race), color_(color) {
}

void RaceTracker::Record(const std::map<std::string, Position>& positions) {
  for (auto& p : positions) {
    if (indexes_.find(p.first) == indexes_.end()) {
      indexes_[p.first] = enemies_.size();
      enemies_.push_back(EnemyTracker(car_tracker_, race_,  p.first, p.second));
    } else {
      enemies_[indexes_[p.first]].RecordPosition(p.second);
    }
  }
}

void RaceTracker::RecordLapTime(const std::string& color, int time) {
  if (indexes_.find(color) == indexes_.end())
    return;

  enemies_[indexes_[color]].RecordLapTime(time);
}

void RaceTracker::RecordCrash(const std::string& color) {
  if (indexes_.find(color) == indexes_.end())
    return;

  enemies_[indexes_[color]].RecordCrash();
}

// TODO test
std::vector<std::string> RaceTracker::CarsBetween(int from, int to, int lane) {
  std::vector<std::string> result;
  for (auto& i : indexes_) {
    if (i.first == color_) continue;
    if (race_.track().IsBetween(enemies_[i.second].state().position(), from, to) &&
        enemies_[i.second].state().position().end_lane() == lane)
      result.push_back(i.first);
  }
  return result;
}

std::vector<std::string> RaceTracker::PredictedCarsBetween(int from, int to, int lane) {
  auto& me = enemies_[indexes_[color_]];
  Position position;
  position.set_piece(to);
  position.set_piece_distance(0);
  int time = me.TimeToPosition(position);

  std::vector<std::string> result;
  for (auto& i : indexes_) {
    if (i.first == color_) continue;
    auto& enemy = enemies_[i.second];

    // If Im already ahead - ignore
    if (race_.track().IsFirstInFront(me.state().position(), enemy.state().position()))
      continue;

    // Check lane
    if (enemy.state().position().end_lane() == lane) {

      // If dead, check if he respawns after I pass him
      if (enemy.is_dead() && race_.track().IsFirstInFront(
            me.PositionAfterTime(enemy.time_to_spawn() - 3),
            enemy.state().position()))
        continue;

      if (race_.track().IsBetween(enemy.PositionAfterTime(time), from, to))
        result.push_back(i.first);
    }
  }
  return result;
}

bool RaceTracker::IsSafeInFront(const Command& command, Command* safe_command) {
  const auto& my_state = car_tracker_.current_state();

  std::map<std::string, CarState> states;
  for (const auto& enemy : enemies_) {
    if (enemy.color() == color_) {
      states[color_] = car_tracker_.Predict(my_state, command);
      continue;
    }

    if (enemy.is_dead()) {
      continue;
    }

    states[enemy.color()] = car_tracker_.Predict(enemy.state(), Command(0));
  }
  const double kCarLength = race_.cars().at(0).length();

  std::set<string> cars_bumped;
  for (int i = 0; i < 100; ++i) {
    CarState my_prev = states[color_];
    Command c(0);
    if (i == 0) { c = command; }
    CarState my_new = car_tracker_.Predict(my_prev, c);
    states[color_] = my_new;

    bool bumped = false;
    double min_velocity = 100000.0;
    for (const auto& p : states) {
      if (p.first == color_) continue;

      double velocity = 0.0;
      Position bump_position = car_tracker_.PredictPosition(my_new.position(), kCarLength);
      if (car_tracker_.MinVelocity(p.second, i + 1, bump_position, &velocity)) {
        // std::cout << "possible bump in " << i + 1 << " ticks" << std::endl;
        // std::cout << "min_velocity = " << velocity << std::endl;
        // If the velocity is higher than ours, he probably was behind us.
        if (velocity < my_new.velocity()) {
          bumped = true;
          min_velocity = fmin(min_velocity, velocity);
        }
      }
    }

    if (!bumped) continue;

    CarState state = my_new;
    state.set_velocity(0.8 * min_velocity);
    if (!car_tracker_.IsSafe(state)) {
      if (fabs(my_state.position().angle()) < 7) {
        // std::cout << "decided after " << i << " ticks" << std::endl;
        // std::cout << "State that is dangerous: " << std::endl;
        // std::cout << my_new.DebugString();
        // std::cout << "min_velocity * 0.8 = " << min_velocity * 0.8 << std::endl;
      }
      std::cout << "WE ARE TOO CLOSE AND WILL DIE. Slowing down." << std::endl;
      *safe_command = Command(0);
      return false;
    }
  }

  // We survived 100 ticks, we should be ok.
  return true;
}

void RaceTracker::FinishedRace(const std::string& color) {
  if (indexes_.find(color) == indexes_.end())
    return;
  enemies_[indexes_[color]].FinishedRace();
}

void RaceTracker::DNF(const std::string& color) {
  if (indexes_.find(color) == indexes_.end())
    return;
  enemies_[indexes_[color]].DNF();
}

void RaceTracker::ResurrectCars() {
  for (auto& enemy : enemies_)
    enemy.Resurrect();
}

bool RaceTracker::ShouldTryToOvertake(const std::string& color, int from, int to) {
  return enemy(color_).CanOvertake(enemy(color), from, to);
}

/* Position RaceTracker::BumpPosition(const std::string& color) {
  int index = indexes_[color];

  // TODO optimize:D
  for (int i = 0; i < 100; i++) {
    auto me = enemies_[indexes_[color_]].PositionAfterTime(i);
    auto he = enemies_[indexes_[color]].PositionAfterTime(i);

    if ((me.piece() == he.piece() && me.piece_distance() > he.piece_distance())
        || (me.piece() + 1) % race_.track().pieces().size() == he.piece())
      return me;
  }
  // This shouldnt reach here
  return enemies_[indexes_[color_]].state().position();
} */

}  // namespace game
