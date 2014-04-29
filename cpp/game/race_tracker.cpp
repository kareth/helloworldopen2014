#include "game/race_tracker.h"

using game::Position;
using game::Race;

namespace game {

RaceTracker::RaceTracker(game::CarTracker& car_tracker,
          const game::Race& race, const std::string& color)
  : car_tracker_(car_tracker), race_(race), color_(color) {
}

void RaceTracker::Record(const std::map<std::string, Position>& positions) {
  DetectBumps(positions);
  for (auto& p : positions) {
    if (indexes_.find(p.first) == indexes_.end()) {
      indexes_[p.first] = enemies_.size();
      enemies_.push_back(EnemyTracker(car_tracker_, race_,  p.first, p.second));
    } else {
      enemies_[indexes_[p.first]].RecordPosition(p.second);
    }
  }
}

void RaceTracker::DetectBumps(const std::map<std::string, Position>& positions) {
  bumps_.clear();

  const double kCarLength = race_.cars()[0].length();
  for (auto& a : positions) {
    if (indexes_.find(a.first) == indexes_.end()) continue;
    for (auto& b : positions) {
      if (indexes_.find(b.first) == indexes_.end()) continue;

      if (a.first == b.first) continue;
      if (enemy(a.first).is_dead() ||
          enemy(b.first).is_dead())
        continue;

      double distance = car_tracker_.DistanceBetween(a.second, b.second);
      if (distance < 40.0) {
        bumps_.push_back({ a.first, b.first });
        printf("Bump detected! %s %s\n", a.first.c_str(), b.first.c_str());
      }
    }
  }
}

bool RaceTracker::BumpOccured(const std::string& color, const std::string& color2) {
  for (auto& b : bumps_) {
    if ((b.first == color && b.second == color2) ||
        (b.second == color && b.first == color2))
      return true;
  }
  return false;
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

// Check if I can die by just going all out
bool RaceTracker::IsSafeAttack(const Command& command, Command* safe_command) {
  return IsSafe(command, safe_command, Command(1));
}

// Check if I can die if he brakes and me either
bool RaceTracker::IsSafeInFront(const Command& command, Command* safe_command) {
  return IsSafe(command, safe_command, Command(0));
}

bool RaceTracker::IsSafe(const Command& command, Command* safe_command, const Command& our_command) {
  const auto& my_state = car_tracker_.current_state();
  bool attack = our_command.throttle() == 1.0;

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

  bool middle_state_not_safe = false;
  bool bump_inevitable = false;
  std::set<string> cars_bumped;
  for (int i = 0; i < 100; ++i) {
    CarState my_prev = states[color_];
    CarState my_new = car_tracker_.Predict(my_prev, our_command);
    states[color_] = my_new;

    if (!car_tracker_.crash_model().IsSafe(my_new.position().angle())) {
      *safe_command = Command(0);
      return false;
    }

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
          cars_bumped.insert(p.first);
          bumped = true;
          min_velocity = fmin(min_velocity, velocity);

          // TODO make sure he actually will crash
        }
      } else {
        // This means, he is not able to run away from us, so there is no point
        // in simulating more ticks.
        if (cars_bumped.count(p.first) > 0) {
          bump_inevitable = true;
        }
      }
    }

    if (bump_inevitable) {
      std::cout << "He is not able to run away from us, we got this :)." << std::endl;
      break;
    }
    if (!bumped) continue;

    if (!car_tracker_.IsSafe(my_new)) {
      middle_state_not_safe = true;
    }

    CarState state = my_prev;
    state.set_velocity(0.8 * min_velocity);
    if (!car_tracker_.IsSafe(state)) {
      std::cout << "WE ARE TOO CLOSE AND WILL DIE. Slowing down (prev)." << std::endl;
      *safe_command = Command(0);
      return false;
    }

    state = my_new;
    state.set_velocity(0.8 * min_velocity);
    if (!car_tracker_.IsSafe(state)) {
      std::cout << "WE ARE TOO CLOSE AND WILL DIE. Slowing down (new)." << std::endl;
      *safe_command = Command(0);
      return false;
    }
  }

  if (middle_state_not_safe && !bump_inevitable) {
    std::cout << "middle_state_not_safe && !bump_inevitable" << std::endl;
    *safe_command = Command(0);
    return false;
  }

  // We survived 50 ticks, we should be ok.
  return true;
}

bool RaceTracker::IsSafeBehind(const Command& command, Command* safe_command) {
  const auto& my_state = car_tracker_.current_state();
  const double kCarLength = race_.cars().at(0).length();

  // TODO check if he is even able to get to us first!

  for (const auto& enemy : enemies_) {
    if (enemy.color() == color_) {
      // states[color_] = car_tracker_.Predict(my_state, command);
      continue;
    }

    if (enemy.is_dead()) {
      continue;
    }

    double distance = car_tracker_.DistanceBetween(enemy.state().position(), my_state.position());
    if (distance > 400) {
      // std::cout << "He is too far, assuming safe" << std::endl;
      continue;
    }

    CarState my_prev = car_tracker_.Predict(my_state, command);
    CarState his_prev = enemy.state();
    // Assume he has a turbo.
    if (his_prev.turbo_state().is_on()) {
      his_prev = car_tracker_.Predict(his_prev, Command(1));
    } else {
      // TODO use last turbo we know about instead of hardcoded 30, 3.
      if (!his_prev.turbo_state().available())
        his_prev.AddNewTurbo(Turbo(30, 3));
      his_prev = car_tracker_.Predict(his_prev, Command::Turbo());
    }
    for (int i = 0; i < 30; ++i) {
      CarState my_new = car_tracker_.Predict(my_prev, Command(0));
      CarState his_new = car_tracker_.Predict(his_prev, Command(1));

      // BUMP
      if (car_tracker_.DistanceBetween(his_new.position(), my_new.position()) < kCarLength ||
          car_tracker_.DistanceBetween(my_new.position(), his_new.position()) < 100) {
        CarState tmp = my_new;
        tmp.set_velocity(0.9 * his_new.velocity());
        if (!car_tracker_.IsSafe(tmp)) {
          std::cout << "Not safe, he can turbo bump me :(" << std::endl;
          *safe_command = Command(0);
          return false;
        }
        break;
      }

      if (!car_tracker_.crash_model().IsSafe(his_new.position().angle()))
        break;

      my_prev = my_new;
      his_prev = his_new;
    }
  }

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

void RaceTracker::TurboForEveryone(const game::Turbo& turbo) {
  for (auto& enemy : enemies_)
    enemy.NewTurbo(turbo);
}

void RaceTracker::CarSpawned(const std::string& color) {
  enemy(color).Spawned();
}

void RaceTracker::TurboStarted(const std::string& color) {
  enemy(color).TurboStarted();
}

bool RaceTracker::WorthBumping(const std::string& color) {
  // TODO
  if (enemy(color).is_dead())
    return false;
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
