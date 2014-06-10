#include "game/race_tracker.h"

using game::Position;
using game::Race;

DECLARE_double(turbo_bump_treshold);

namespace game {

RaceTracker::RaceTracker(game::CarTracker& car_tracker,
          const game::Race& race, const std::string& color)
  : car_tracker_(car_tracker), race_(race), color_(color), enemies_(),
    bump_detector_(car_tracker, race),
    lane_scorer_(race, car_tracker, *this, enemies_, color) {
}

void RaceTracker::Record(const std::map<std::string, Position>& positions, int game_tick) {
  bump_detector_.Record(enemies_, positions, game_tick);
  for (auto& p : positions) {
    if (indexes_.find(p.first) == indexes_.end()) {
      indexes_[p.first] = enemies_.size();
      enemies_.push_back(EnemyTracker(car_tracker_, race_,  p.first, p.second));
    } else {
      enemies_[indexes_[p.first]].RecordPosition(p.second);
    }
  }
}

bool RaceTracker::HasBumped(
    const Position& position1,
    const Position& position2) {
  const double kCarLength = race_.cars()[0].length();

  if (position1.start_lane() != position2.start_lane() ||
      position1.end_lane() != position2.end_lane()) {
    return false;
  }

  return car_tracker_.DistanceBetween(position1, position2, nullptr, kCarLength + 1) <= kCarLength + 1e-9 ||
         car_tracker_.DistanceBetween(position2, position1, nullptr, kCarLength + 1) <= kCarLength + 1e-9;
}

bool RaceTracker::HasSomeoneMaybeBumpedMe(const map<string, Position>& positions, double* bump_velocity) {
  const double kCarLength = race_.cars().at(0).length();

  const auto& my_position = positions.find(color_)->second;
  for (const auto& p : positions) {
    std::string color = p.first;
    const auto& position = p.second;

    // Ignore enemies that do not exist any more (that is strange?).
    if (indexes_.find(color) == indexes_.end()) {
      continue;
    }

    // Ignore enemies that are dead or have finished.
    const auto& e = enemies_[indexes_[color]];
    if (e.is_dead() || e.has_finished()) continue;

    if (HasBumped(my_position, position)) {
      if (car_tracker_.DistanceBetween(position, my_position, nullptr, kCarLength + 1) <= kCarLength + 1e-9) {
        // If he bumped me.
        *bump_velocity = car_tracker_.velocity_model().Predict(enemy(color).state().velocity(), 0) * 0.9;
      } else {
        // If I bumped him.
        *bump_velocity = car_tracker_.velocity_model().Predict(enemy(color).state().velocity(), 0) * 0.8;
      }
      return true;
    }
  }

  return false;
}

bool RaceTracker::BumpOccured(const std::string& color, const std::string& color2) {
  return bump_detector_.BumpOccured(color, color2);
}

std::map<Switch, int> RaceTracker::ScoreLanes(const CarState& state, const utils::Deadline& deadline) {
  int from = race_.track().NextSwitch(state.position().piece());
  int to = race_.track().NextSwitch(from);
  int lane = state.position().end_lane();

  std::map<Switch, int> scores;
  for (int offset = -1; offset <= 1; offset++) {
    int new_lane = state.position().end_lane() + offset;
    if (race_.track().IsLaneCorrect(new_lane))
      scores[Switch(offset+1)] = lane_scorer_.ScoreLane(from, to, new_lane, deadline);
  }
  return scores;
}

// TODO OBSOLETE
// Is the car slow enough to overtake ?
bool RaceTracker::ShouldOvertake(const std::string& color, int from, int to) {
 if (enemy(color).is_dead()) {
    if (race_.track().IsFirstInFront(
        enemy(color_).PositionAfterTime(enemy(color).time_to_spawn() - 15),
        enemy(color).state().position()))
      return false;
    else
      return true;
  }

  // Overtake everyone in the beginning
  if (car_tracker_.current_state().position().lap() < 1)
    return true;

  return enemy(color_).CanOvertake(enemy(color), from, to);
}

// Is it worth turbo bumping?
bool RaceTracker::WorthBumping(const std::string& color) {
  if (enemy(color).is_dead())
    return false;

  return (enemy(color).best_lap() * FLAGS_turbo_bump_treshold < enemy(color_).best_lap());
}

// TODO move?

// Check if I can die by just going all out
bool RaceTracker::IsSafeAttack(const CarState& current_state, const Command& command, Command* safe_command) {
  return IsSafe(current_state, command, safe_command, Command(1));
}

// Check if I can die if he brakes and me either
bool RaceTracker::IsSafeInFront(const CarState& current_state, const Command& command, Command* safe_command) {
  return IsSafe(current_state, command, safe_command, Command(0));
}

bool RaceTracker::IsSafeAhead(const CarState& current_state, const Command& command, Command* safe_command) {
  const double kCarLength = race_.cars().at(0).length();

  vector<CarState> states{current_state, car_tracker_.Predict(current_state, command)};
  if (!car_tracker_.GenerateSafeStates(states.back(), &states)) {
    std::cout << "IsSafeAhead: Could not found safe mask." << std::endl;
    *safe_command = command;
    return true;
  }

  for (int ticks_after = 1; ticks_after < states.size(); ++ticks_after) {
    const auto& my_state = states[ticks_after];

    bool bumped = false;
    bool bump_inevitable = false;

    double min_velocity = 100000.0;
    for (const auto& enemy : enemies_) {
      if (enemy.color() == color_) continue;
      if (enemy.has_finished()) continue;

      // FIXME
      // Replace it with:
      // if (!enemy.IsThreatTo(me)) continue;
      if (enemy.is_dead()) continue;

      double velocity = 0.0;
      if (car_tracker_.CanBumpAfterNTicks(my_state, enemy.state(), ticks_after, &velocity)) {
        bumped = true;
        min_velocity = fmin(min_velocity, velocity);
      }

      // Have I already bumped him and the bump is inevitable so it doesn't make sense
      // to simulate more.
      if (car_tracker_.IsBumpInevitable(states[0], my_state, enemy.state(), ticks_after)) {
        std::cout << "IsSafeAhead: Bump is inevitable" << std::endl;
        bump_inevitable = true;
      }
    }

    if (!bumped && bump_inevitable) break;
    if (!bumped) continue;

    CarState state = my_state;
    state.set_velocity(0.8 * min_velocity);

    // std::cout << "Predicted state after bump: " << state.ShortDebugString() << std::endl;

    if (!car_tracker_.GenerateSafeStates(state, nullptr)) {
      *safe_command = Command(0);

      // Check, if issuing Command(0) is safe. If not, it is better
      // to issue whatever command we were planning to issue.
      // We check for Command(0) here, to avoid inifinite recurrence.
      if (command.ThrottleSet() && command.throttle() != 0.0) {
        if (!IsSafeAhead(current_state, Command(0), safe_command)) {
          std::cout << "IsSafeAhead: We couldn't find safe_command. Use scheduled command" << std::endl;
          *safe_command = command;
        }
      }

      return false;
    }

    if (bump_inevitable) break;
  }

  // We survived 100 ticks, we should be ok.
  return true;
}

// command - the first command that we should issue to 'current_state'
// our_command - the command that we should issue to 'current_state' after first tick
// safe_command - if this method return false, safe_command will contain command that is safe.
bool RaceTracker::IsSafe(const CarState& current_state, const Command& command,
                         Command* safe_command, const Command& our_command) {
  bool attack = our_command.throttle() == 1.0;
  const double kCarLength = race_.cars().at(0).length();

  bool middle_state_not_safe = false;
  bool bump_inevitable = false;
  bool attack_not_successful = false;
  std::set<string> cars_bumped;

  auto my_new = current_state;
  for (int ticks_after = 0; ticks_after < 100; ++ticks_after) {
    CarState my_prev = my_new;
    Command c = ticks_after == 0 ? command : our_command;
    my_new = car_tracker_.Predict(my_prev, c);

    if (!car_tracker_.crash_model().IsSafe(my_new.position().angle())) {
      *safe_command = Command(0);
      return false;
    }

    bool bumped = false;
    double min_velocity = 100000.0;
    for (const auto& enemy : enemies_) {
      if (enemy.color() == color_) continue;
      if (enemy.is_dead()) continue;

      double velocity = 0.0;
      int full_throttle_ticks = 0;
      Position bump_position = car_tracker_.PredictPosition(my_new.position(), kCarLength);
      if (car_tracker_.MinVelocity(enemy.state(), ticks_after + 1, bump_position, &velocity, &full_throttle_ticks)) {
        // std::cout << "possible bump in " << i + 1 << " ticks" << std::endl;
        // std::cout << "min_velocity = " << velocity << std::endl;
        // If the velocity is higher than ours, he probably was behind us.
        if (velocity < my_new.velocity()) {
          cars_bumped.insert(enemy.color());
          bumped = true;
          min_velocity = fmin(min_velocity, velocity);

          // TODO make sure he actually will crash
          if (attack) {
            CarState s = enemy.state();
            for (int i = 0; i < full_throttle_ticks; ++i) { s = car_tracker_.Predict(s, Command(1)); }
            for (int i = 0; i < ticks_after - full_throttle_ticks; ++i) { s = car_tracker_.Predict(s, Command(0)); }
            s.set_velocity(my_new.velocity() * 0.9);
            if (car_tracker_.IsSafe(s)) {
              attack_not_successful = true;
            }
          }
        }
      } else {
        // This means, he is not able to run away from us, so there is no point
        // in simulating more ticks.
        if (cars_bumped.count(enemy.color()) > 0) {
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
      if (our_command.throttle() != 1) {
        std::cout << "WE ARE TOO CLOSE AND WILL DIE. Slowing down (prev)." << std::endl;
      }
      *safe_command = Command(0);
      return false;
    }

    state = my_new;
    state.set_velocity(0.8 * min_velocity);
    if (!car_tracker_.IsSafe(state)) {
      if (our_command.throttle() != 1) {
        std::cout << "WE ARE TOO CLOSE AND WILL DIE. Slowing down (new)." << std::endl;
      }
      *safe_command = Command(0);
      return false;
    }
  }

  if (middle_state_not_safe && !bump_inevitable) {
    std::cout << "middle_state_not_safe && !bump_inevitable" << std::endl;
    *safe_command = Command(0);
    return false;
  }

  if (attack && attack_not_successful) {
    std::cout << "Attack would not crash the opponent." << std::endl;
    *safe_command = command;
    return false;
  }

  // We survived 50 ticks, we should be ok.
  return true;
}

bool RaceTracker::IsSafeBehind(const CarState& current_state,
                               const vector<double>& schedule,
                               const Command& command,
                               Command* safe_command) {
  const double kCarLength = race_.cars().at(0).length();
  *safe_command = command;

  vector<CarState> states{current_state};
  for (double throttle : schedule) {
    states.push_back(car_tracker_.Predict(states.back(), Command(throttle)));
  }

  for (int ticks_after = 1; ticks_after < states.size(); ++ticks_after) {
    const auto& my_state = states[ticks_after];
    for (const auto& enemy : enemies_) {
      if (enemy.color() == color_) continue;
      if (enemy.is_dead()) continue;
      if (enemy.has_finished()) continue;

      if (car_tracker_.DistanceBetween(enemy.state().position(), my_state.position()) > 200) {
        // std::cout << "Car too far away to attack us. " << std::endl;
        continue;
      }

      double velocity = 0.0;
      // std::cout << "ticks_after: " << ticks_after << std::endl;
      // std::cout << "enemy_state: " << enemy.state().ShortDebugString() << std::endl;
      // std::cout << "my_state: " << my_state.ShortDebugString() << std::endl;
      if (car_tracker_.MaxVelocity(enemy.state(), my_state, ticks_after, &velocity)) {
        std::cout << "Enemy can hit us in " << ticks_after << " ticks with velocity " << velocity << std::endl;
        CarState tmp = my_state;
        tmp.set_velocity(0.9 * velocity);
        if (!car_tracker_.GenerateSafeStates(tmp, nullptr)) {
          std::cout << "Enemy can bump and crash us in " << ticks_after << " ticks" << std::endl;
          vector<CarState> safe_states;
          if (!car_tracker_.GenerateSafeStates(car_tracker_.Predict(current_state, Command(0)), &safe_states)) {
            std::cout << "Issueing command 0 would crash us :(. No possible defense" << std::endl;
            return false;
          }
          vector<double> zero_throttles(safe_states.size(), 0.0);
          if (schedule[0] != 0.0) {
            std::cout << "Second IsSafeBehind to make sure Command(0) will help." << std::endl;
            if (IsSafeBehind(current_state, zero_throttles, command, safe_command)) {
              std::cout << "Command(0) will save us." << std::endl;
              *safe_command = Command(0);
              return false;
            } else {
              std::cout << "IsSafeBehind: Command(0) is also not safe." << std::endl;
              return false;
            }
          } else {
            std::cout << "We already issue command 0 and we still can be bumped. No defense." << std::endl;
            return false;
          }
        }
      }
    }
  }

  return true;
}

// Record methods

void RaceTracker::RecordLapTime(const std::string& color, int time) {
  enemy(color).RecordLapTime(time);
}

void RaceTracker::TurboForEveryone(const game::Turbo& turbo) {
  for (auto& enemy : enemies_)
    enemy.NewTurbo(turbo);
}

void RaceTracker::CarSpawned(const std::string& color) {
  enemy(color).Spawned();
}

void RaceTracker::RecordCrash(const std::string& color) {
  enemy(color).RecordCrash();
}

void RaceTracker::TurboStarted(const std::string& color) {
  enemy(color).TurboStarted();
}

void RaceTracker::FinishedRace(const std::string& color) {
  enemy(color).FinishedRace();
}

void RaceTracker::DNF(const std::string& color) {
  // TODO
  enemy(color).DNF();
}

void RaceTracker::ResurrectCars() {
  for (auto& enemy : enemies_)
    enemy.Resurrect();
}


}  // namespace game
