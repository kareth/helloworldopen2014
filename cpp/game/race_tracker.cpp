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

bool RaceTracker::IsSafe(const Command& command, Command* safe_command) {
  const auto& my_state = car_tracker_.current_state();

  // TODO get only cars after mines within two pieces

  std::map<std::string, CarState> states;
  std::vector<string> cars_tracked;
  double min_velocity = 1000;
  for (const auto& enemy : enemies_) {
    if (enemy.color() == color_) continue;

    if (car_tracker_.DistanceBetween(my_state.position(), enemy.state().position()) < 200) {
      cars_tracked.push_back(enemy.color());
    }

    states[enemy.color()] = enemy.state();
    min_velocity = fmin(min_velocity, enemy.state().velocity());
  }

  if (cars_tracked.size() == 0) {
    std::cout << "No-one is in front of us." << std::endl;
    return true;
  }

  std::cout << "Someone is in front of us." << std::endl;

  CarState state = car_tracker_.Predict(my_state, command);
  state.set_velocity(0.8 * min_velocity);
  bool is_safe = car_tracker_.IsSafe(state);

  if (!is_safe) {
    std::cout << "WE ARE TOO CLOSE AND WILL DIE. Slowing down." << std::endl;
    *safe_command = Command(0);
    return false;
  }

  return true;
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
