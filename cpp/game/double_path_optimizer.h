#ifndef CPP_GAME_DOUBLE_PATH_OPTIMIZER_H_
#define CPP_GAME_DOUBLE_PATH_OPTIMIZER_H_

#include <thread>
#include "game/path_optimizer_interface.h"
#include "game/greedy_path_optimizer.h"
#include "game/perfect_path_optimizer.h"

namespace game {

class DoublePathOptimizer : public PathOptimizerInterface {
 public:
  DoublePathOptimizer(const Race& race, CarTracker& car_tracker);

  // Returns all possible decisions with associated score
  // score means the loss of time (lap-wise) compared to optimal choice
  // so atleast one lane will always have value of 0
  std::map<Switch, int> Score(const Position& position) override;

 private:
  void RunPerfectOptimizer();

  const Race& race_;
  CarTracker& car_tracker_;

  GreedyPathOptimizer greedy_;
  PerfectPathOptimizer perfect_;

  std::unique_ptr<std::thread> optimizer_thread_;

  std::atomic<bool> is_perfect_ready_;
  bool optimizer_started_;
};

}  // namespace game

#endif  // CPP_GAME_GREEDY_PATH_OPTIMIZER_H_
