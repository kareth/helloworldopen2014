#ifndef CPP_GAME_ENEMY_TRACKER_H_
#define CPP_GAME_ENEMY_TRACKER_H_

#include <string>
#include <map>
#include "game/race.h"

namespace game {

class EnemyInfo {
  EnemyInfo(double distance, double speed_factor, int expected_bump_time)
    : distance_(distance), speed_factor_(speed_factor), expected_bump_time_(expected_bump_time) {
  }

  double distance() const { return distance_; }
  double speed_factor() const { return speed_factor_; }
  int expected_bump_time() const { return expected_bump_time_; }

 private:
  double distance_;

  // Ratio of my avg speed to his
  double speed_factor_;

  // Positive - I will bump him soon;
  // Negative - he will bump me soon;
  // 0 - too distant = wont hit him too soon ^^
  int expected_bump_time_;
};

class EnemyTracker {
 public:
  EnemyTracker(const Race& race, const std::string& color);

  void Record(const std::map<std::string, Position>& positions);

  // Resets positions, keeps speed stats.
  void Reset();

  const std::vector<EnemyInfo*>& enemies_ahead() const { return enemies_ahead_; }
  const std::vector<EnemyInfo*>& enemies_behind() const { return enemies_behind_; }

  const EnemyInfo& car_info(const std::string& color) const { return enemies_.at(color); }

 private:
  std::vector<EnemyInfo*> cars_ahead_;
  std::vector<EnemyInfo*> cars_behind_;
  std::map<std::string, EnemyInfo> enemies_;
};

}  // namespace game

#endif  // CPP_GAME_ENEMY_TRACKER_H_
