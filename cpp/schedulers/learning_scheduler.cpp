#include "schedulers/learning_scheduler.h"

namespace schedulers {

LearningScheduler::LearningScheduler(const game::Race& race,
               game::CarTracker& car_tracker,
               int time_limit)
 : race_(race), car_tracker_(car_tracker){
  throttle_scheduler_.reset(
      new BinaryThrottleScheduler(race_, car_tracker_, time_limit));
}

void LearningScheduler::Schedule(const game::CarState& state) {
  throttle_scheduler_->Schedule(state);

  command_ = game::Command(throttle_scheduler_->throttle());

  auto next = car_tracker_.Predict(state, command_);

  if (next.velocity() < state.velocity())
    command_ = game::Command(
        car_tracker_.velocity_model().PredictThrottle(state.velocity()));
}

void LearningScheduler::set_strategy(const Strategy& strategy) {
  strategy_ = strategy;
  throttle_scheduler_->set_strategy(strategy);
}

void LearningScheduler::Overtake(const string& color) {
  printf("NOT IMPLEMENTED\n");
}

void LearningScheduler::IssuedCommand(const game::Command& command) {
}

}  // namespace schedulers
