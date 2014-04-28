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

  // const EnemyTracker& enemy(const std::string& color) const { return enemies_.at(indexes_.at(color)); }

  /* int TimeToReach(int piece, double position);

  Position BumpPosition(const std::string& color);*/

  // from to inclusive
  std::vector<std::string> CarsBetween(int piece1, int piece2, int lane);

  std::vector<std::string> PredictedCarsBetween(int from, int to, int lane);

  // Detects if issueing the given command is safe based on cars
  // in front of us. If false, also returns command that is safe in safe_command.
  //
  // It only checks cars that are ahead of us (200 units), and makes sure that
  // if we bump them, we will not crash.
  //
  // What is not working:
  // - if someone is much slower, and will take a switch just before us and
  //   we bump into him.
  // - if we ride on switch (or someone is on switch)
  bool IsSafeInFront(const Command& command, Command* safe_command);

  bool IsSafeAttack(const Command& command, Command* safe_command);

  bool IsSafeBehind(const Command& command, Command* safe_command);

  void FinishedRace(const std::string& color);
  void DNF(const std::string& color);

  void ResurrectCars();

  bool ShouldTryToOvertake(const std::string& color, int from, int to);

  EnemyTracker& enemy(const std::string& color) { return enemies_[indexes_[color]]; }

  void TurboForEveryone(const game::Turbo& turbo);
  void CarSpawned(const std::string& color);
  void TurboStarted(const std::string& color);

  const std::vector<EnemyTracker>& enemies() const { return enemies_; }

  bool WorthBumping(const std::string& color);

 private:
  bool IsSafe(const Command& command, Command* safe_command, const Command& our_command);
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
