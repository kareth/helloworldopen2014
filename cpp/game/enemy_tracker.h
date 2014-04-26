#ifndef CPP_GAME_ENEMY_TRACKER_H_
#define CPP_GAME_ENEMY_TRACKER_H_

#include <string>
#include <map>
#include "game/race.h"

namespace game {

class EnemyTracker {
  EnemyInfo(double distance, double speed_factor, int expected_bump_time, const CarState& state)
    : distance_(distance), speed_factor_(speed_factor), expected_bump_time_(expected_bump_time), state_(state) {
  }

  double distance() const { return distance_; }
  double speed_factor() const { return speed_factor_; }


  int expected_bump_time() const { return expected_bump_time_; }
  std::pair<int, double> expected_bump_position() const { return expected_bump_position_; }
  const CarState& state() const { return state_; }

 private:
  double distance_;

  // Ratio of my avg speed to his
  double speed_factor_;

  // Positive - I will bump him soon;
  // Negative - he will bump me soon;
  // INF?? - too distant = wont hit him too soon ^^
  int expected_bump_time_;

  // <piece, in_piece_distance>
  std::pair<int, double> expected_bump_position_;

  CarState state_;
};
}  // namespace game

#endif  // CPP_GAME_ENEMY_TRACKER_H_
