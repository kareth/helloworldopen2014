#ifndef CPP_GAME_RACE_TRACKER_H_
#define CPP_GAME_RACE_TRACKER_H_

#include "game/race.h"
#include "game/enemy_tracker.h"
#include "game/position.h"
#include "game/car_tracker.h"
#include "game/bump_detector.h"
#include "game/lane_scorer.h"

namespace game {

class RaceTracker {
 public:
  RaceTracker(game::CarTracker& car_tracker,
              const game::Race& race, const std::string& color);

  // Detects if issuing the given command is safe based on cars
  // in front of us. If false, also returns command that is safe in safe_command.
  //
  // It only checks cars that are ahead of us (200 units), and makes sure that
  // if we bump them, we will not crash.
  //
  // What is not working:
  // - if someone is much slower, and will take a switch just before us and
  //   we bump into him.
  // - if we ride on switch (or someone is on switch)
  bool IsSafeInFront(const CarState& current_state, const Command& command, Command* safe_command);

  bool IsSafeAttack(const CarState& current_state, const Command& command, Command* safe_command);

  bool IsSafeBehind(const CarState& current_state, const Command& command, Command* safe_command);

  // Recording methods

  void Record(const std::map<std::string, game::Position>& positions);
  void RecordCrash(const std::string& color);
  void RecordLapTime(const std::string& color, int time);
  void FinishedRace(const std::string& color);
  void TurboForEveryone(const game::Turbo& turbo);
  void CarSpawned(const std::string& color);
  void TurboStarted(const std::string& color);
  void DNF(const std::string& color);
  void ResurrectCars();

  // Lane scores based on who is there
  std::map<Switch, int> ScoreLanes(const CarState& state);

  // TODO
  // Move to enemy?
  bool WorthBumping(const std::string& color);
  bool ShouldOvertake(const std::string& color, int from, int to);

  bool IsCompetitive(const std::string& color) { return false; }

  // Getters
  const std::vector<EnemyTracker>& enemies() const { return enemies_; }
  const std::string& my_color() const { return color_; }
  EnemyTracker& enemy(const std::string& color) { return enemies_[indexes_[color]]; }

  // Returns true if there was bump between those two cars in last tick
  bool BumpOccured(const std::string& color, const std::string& color2);

 private:
  bool IsSafe(const CarState& state, const Command& command,
              Command* safe_command, const Command& our_command);

  void DetectBumps(const std::map<std::string, Position>& positions);

  // void RecordEnemy(int index, const game::Position& position);
  // void UpdateSpeedStats(int index, const game::Position& position);

  std::vector<EnemyTracker> enemies_;
  // Maps color to index
  std::map<std::string, int> indexes_;

  const Race& race_;
  CarTracker& car_tracker_;
  std::string color_;

  BumpDetector bump_detector_;
  LaneScorer lane_scorer_;
};

}  // namespace game

#endif  // CPP_GAME_RACE_TRACKER_H_
