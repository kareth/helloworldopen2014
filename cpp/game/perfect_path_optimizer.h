#ifndef CPP_GAME_PERFECT_PATH_OPTIMIZER_H_
#define CPP_GAME_PERFECT_PATH_OPTIMIZER_H_

#include <algorithm>
#include "game/path_optimizer_interface.h"
#include "game/velocity_predictor.h"
#include <thread>

namespace bots {
namespace switch_optimizer {
  class Bot;
}
}

namespace game {

class Simulator;

class PerfectPathOptimizer : public PathOptimizerInterface {
 public:
  PerfectPathOptimizer(const Race& race, const PhysicsParams& physics);

  // Returns all possible decisions with associated score
  // score means the loss of time (lap-wise) compared to optimal choice
  // so atleast one lane will always have value of 0
  std::map<Switch, int> Score(const Position& position) override;

  void Optimize(std::atomic<bool>* ready_flag);

 private:
  double LapLength(int piece, int lane);
  void ParseBotData(int lane, bots::switch_optimizer::Bot* bot);
  double TimeOnPieceBetween(const CarState& a, const CarState& b, int piece);

  void SimulateLanes();
  void ComputeScores();

  // [piece][lane]
  vector<vector<double>> lane_times_;
  vector<vector<map<Switch, double>>> lane_scores_;

  VelocityPredictor velocity_predictor_;

  // IMPORTANT: COPIES
  Race race_;
  CarTracker car_tracker_;
  PhysicsParams physics_;
};

}  // namespace game

#endif  // CPP_GAME_GREEDY_PATH_OPTIMIZER_H_
