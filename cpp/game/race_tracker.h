#ifndef CPP_GAME_RACE_TRACKER_H_
#define CPP_GAME_RACE_TRACKER_H_

#include "game/race.h"
#include "game/enemy_tracker.h"
#include "game/position.h"
#include "game/car_tracker.h"

namespace game {

class RaceTracker {
 public:
  RaceTracker(game::CarTracker& car_tracker,
              const game::Race& race, const std::string& color);

  void Record(const std::map<std::string, game::Position>& positions);

  void RecordLapTime(const std::string& color, int time);

  void RecordCrash(const std::string& color);

  // Resets positions, keeps speed stats.
  // TODO is it necessary?
  // void Reset() {}

  const EnemyTracker& enemy(const std::string& color) const { return enemies_.at(indexes_.at(color)); }

  /* int TimeToReach(int piece, double position);

  Position BumpPosition(const std::string& color);*/

  // from to inclusive
  std::vector<std::string> CarsBetween(int piece1, int piece2, int lane);

  std::vector<std::string> PredictedCarsBetween(int from, int to, int lane);

  // If false, returns command that is safe.
  bool IsSafe(const Command& command, Command* safe_command);

 private:
  // void RecordEnemy(int index, const game::Position& position);
  // void UpdateSpeedStats(int index, const game::Position& position);

  std::vector<EnemyTracker> enemies_;

  std::map<std::string, int> indexes_;

  const Race& race_;
  CarTracker& car_tracker_;
  std::string color_;
};

}  // namespace game

#endif  // CPP_GAME_RACE_TRACKER_H_
