#ifndef CPP_GAME_ENEMY_TRACKER_H_
#define CPP_GAME_ENEMY_TRACKER_H_

#include <string>
#include <map>
#include "game/race.h"

namespace game {

class EnemyInfo {
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

class EnemyTracker {
 public:
  EnemyTracker(const game::Race& race, const std::string& color);

  void Record(const std::map<std::string, game::Position>& positions);

  void RecordLapTime(const std::string& color, int time);

  // Resets positions, keeps speed stats.
  // TODO is it necessary?
  void Reset() {}

  const std::vector<EnemyInfo*>& enemies_ahead() const { return enemies_ahead_; }
  const std::vector<EnemyInfo*>& enemies_behind() const { return enemies_behind_; }

  const EnemyInfo& car_info(const std::string& color) const { return enemies_.at(indexes_.at(color)); }

  int TimeToReach(int piece, double position);

 private:
  void RecordEnemy(int index, const game::Position& position);
  void UpdateSpeedStats(int index, const game::Position& position);

  std::vector<EnemyInfo*> cars_ahead_;
  std::vector<EnemyInfo*> cars_behind_;

  std::vector<EnemyInfo> enemies_;
  std::vector<std::vector<int>> lap_times_;


  std::map<std::string, int> indexes_;
};

}  // namespace game

#endif  // CPP_GAME_ENEMY_TRACKER_H_
