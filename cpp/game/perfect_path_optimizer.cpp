#include "game/perfect_path_optimizer.h"

namespace game {

PerfectPathOptimizer::PerfectPathOptimizer(const Race& race, const PhysicsParams& physics)
  : race_(race), physics_(physics) {
}

std::map<Switch, int> PerfectPathOptimizer::Score(const Position& position) {
  printf("Asking for perfect score\n");
  std::map<Switch, int> m;
  return m;
}

void PerfectPathOptimizer::Optimize(std::atomic_bool* ready_flag) {
  printf("---------- Switch Optimizer Started!\n");
  sleep(5);
  //ready_flag->store(true);
  printf("---------- Optimizer Finished!\n");
}

}  // namespace game
