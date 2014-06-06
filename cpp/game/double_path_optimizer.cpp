#include "double_path_optimizer.h"

namespace game {


DoublePathOptimizer::DoublePathOptimizer(const Race& race, CarTracker& car_tracker)
  : race_(race), car_tracker_(car_tracker),
    greedy_(race, car_tracker),
    perfect_(race, car_tracker) {
  is_perfect_ready_.store(false);
  optimizer_started_ = false;
}

std::map<Switch, int> Score(const Position& position) override {
  if (is_perfect_ready_.load() == true) {
    return perfect_.Score(position);
  } else {
    if (car_tracker_.IsReady()) {
      RunPerfectOptimizer();
      optimizer_started_ = true;
    }

    return greedy_.Score(position);
  }
}

void DoublePathOptimizer::RunPerfectOptimizer() {

}

}  // namespace game
