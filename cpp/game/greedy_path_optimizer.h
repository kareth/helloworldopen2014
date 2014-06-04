#ifndef CPP_GAME_GREEDY_PATH_OPTIMIZER_H_
#define CPP_GAME_GREEDY_PATH_OPTIMIZER_H_

#include "game/path_optimizer_interface.h"

namespace game {

class GreedyPathOptimizer : public PathOptimizerInterface {
 public:
  GreedyPathOptimizer(const Race& race, CarTracker& car_tracker);

  // Returns all possible decisions with associated score
  // score means the loss of time (lap-wise) compared to optimal choice
  // so atleast one lane will always have value of 0
  std::map<Switch, int> Score(const Position& position) override;

 private:
  // The distance between 'start' and 'end' assuming that 'end' position is on
  // lane 'lane'.
  double DistanceBetween(const Position& position1, const Position& position2, int lane);

  // Maps -1, 0 1 to kSwitchLeft, stay, right
  Switch OffsetToDirection(int offset);

  const int kApproximateSpeed = 7;

  const Race& race_;
  CarTracker& car_tracker_;
};

}  // namespace game

#endif  // CPP_GAME_GREEDY_PATH_OPTIMIZER_H_
