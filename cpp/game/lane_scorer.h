#ifndef CPP_GAME_LANE_SCORER_H_
#define CPP_GAME_LANE_SCORER_H_

#include "game/enemy_tracker.h"

namespace game {

class RaceTracker;

class LaneScore {
 public:
  LaneScore(int score) : score_(score) {};

  operator int() const { return score_; }

  bool IsEmpty() { return score_ == 0; }
  bool CarToOvertake() { return score_ > 0; }
  bool CarToBump() { return score_ < 0; }

 private:
  int score_;
};

class LaneScorer {
 public:
  LaneScorer(const Race& race,
             CarTracker& car_tracker,
             RaceTracker& race_tracker,
             std::vector<EnemyTracker>& enemies,
             const string& color);

  // = 0 - ignore
  // > 0 - competitive, should bump
  // < 0 - slow, should overtake
  LaneScore ScoreLane(int from, int to, int lane);

 private:
  int ScoreEnemy(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position);
  int ScoreDeadEnemy(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position);
  int ScoreLivingEnemy(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position);
  int EnemyBumpScore(const EnemyTracker& enemy, double my_speed, double his_speed);
  bool BumpPosition(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position, Position* bump_position);

  std::vector<EnemyTracker>& enemies_;
  const Race& race_;
  CarTracker& car_tracker_;
  RaceTracker& race_tracker_;

  const string color_;
  const double kCarLength;

  const int kDeadCrash = -10;
  const int kMustBump = 10;
};

}  // namespace game

#endif  // CPP_GAME_LANE_SCORER_H_
