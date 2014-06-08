#ifndef CPP_GAME_LANE_SCORER_H_
#define CPP_GAME_LANE_SCORER_H_

#include "game/enemy_tracker.h"

namespace game {

class RaceTracker;

class LaneScorer {
 public:
  LaneScorer(const Race& race,
             CarTracker& car_tracker,
             RaceTracker& race_tracker,
             std::vector<EnemyTracker>& enemies,
             const string& color);

  ~LaneScorer();

  // [-100 - 100]
  // negative - slower than us (score of the worst guy there)
  // 0        - neutral (wont bump him)
  // positive - competitive, should bump (score of the best guy there)
  //
  // @lane should be valid track lane
  // @labe should be reachable from my position without doing full lap
  double ScoreLane(int from, int to, int lane);

 private:
  // Returns score for the enemy
  // end_position is the position that I want to be able to reach safely
  // i.e. begin of switch
  double ScoreEnemy(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position);

  // Returns 0 if we can just pass him before he spawns
  // Returns kDeadCrash if he spawns before we pass him
  // Uses 2 * kCarLength additional distance to decide if its safe
  double ScoreDeadEnemy(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position);

  // Checks if bump will occur
  // If yes, returns bumpscore based on cars
  // predicted velocity in the moment of bump
  double ScoreLivingEnemy(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position);

  // -100      for car standing or spawning
  // <-100, 0) for cars that are slower than us by more than 5%
  // 0         for car that is 0-5% slower than us
  // TODO (0-100)   for competitive guys that are worth bumping
  //
  // Scales linearly by far, should probably be other
  // TODO if the guy is just 5% slower, we can possibly just
  //      get stuck behind him. What if we should overtake?
  double EnemyBumpScore(const EnemyTracker& enemy, double my_speed, double his_speed);

  // Returns true if bump will occur
  // result in bump_position as position of the car ahead (enemy)
  //
  // Uses 10% additional car length as safe margin
  // TODO OPTIMIZE its very slow
  bool BumpPosition(const EnemyTracker& me, const EnemyTracker& enemy, const Position& end_position, Position* bump_position);

  std::vector<EnemyTracker>& enemies_;
  const Race& race_;
  CarTracker& car_tracker_;
  RaceTracker& race_tracker_;
  const string color_;  // my color

  const double kCarLength;

  const double kDeadCrash = -100;
  const double kMustBump = 100;

  double longest_calculation_time_ = 0;
};

}  // namespace game

#endif  // CPP_GAME_LANE_SCORER_H_
