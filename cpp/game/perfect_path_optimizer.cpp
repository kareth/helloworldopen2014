#include "game/perfect_path_optimizer.h"

namespace game {

PerfectPathOptimizer::PerfectPathOptimizer(const Race& race, const PhysicsParams& physics)
  : race_(race), physics_(physics) {
}

std::map<Switch, int> PerfectPathOptimizer::Score(const Position& position) {
  return lane_scores_[position.piece()][position.end_lane()];
}

void PerfectPathOptimizer::Optimize(std::atomic_bool* ready_flag) {
  printf("---------- Switch Optimizer Started!\n");
  printf("---------- Starting simulation\n");
  SimulateLanes();
  printf("---------- Simulation done, calculating scores\n");
  ComputeScores();
  //ready_flag->store(true);
  printf("---------- Optimizer Finished!\n");
}

void PerfectPathOptimizer::SimulateLanes() {

}


void PerfectPathOptimizer::ComputeScores() {
  /*lane_scores_.resize(lane_times.size());
  for (int i = 0; i < lane_scores.size(); i++)
  
  for (int piece = 0; piece < lane_times_.size(); piece++) {
    for (int lane = 0; lane < lane_times_[piece].sizee()) {
      lane_scores_[piece][lane] = Score(piece, lane);
    }
  }*/
}

}  // namespace game
