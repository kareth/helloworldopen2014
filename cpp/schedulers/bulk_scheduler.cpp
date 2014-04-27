#include "schedulers/bulk_scheduler.h"

namespace schedulers {

BulkScheduler::BulkScheduler(const game::Race& race,
               game::RaceTracker& race_tracker,
               game::CarTracker& car_tracker,
               int time_limit)
 : race_(race), car_tracker_(car_tracker), race_tracker_(race_tracker) {
  throttle_scheduler_.reset(
      new BinaryThrottleScheduler(race_, car_tracker_, time_limit));
  turbo_scheduler_.reset(
      new GreedyTurboScheduler(race_, car_tracker_));
  switch_scheduler_.reset(
      new ShortestPathSwitchScheduler(race_, race_tracker_, car_tracker_));
}

void BulkScheduler::Schedule(const game::CarState& state) {
  throttle_scheduler_->Schedule(state);
  switch_scheduler_->Schedule(state);
  turbo_scheduler_->Schedule(state);

  if (turbo_scheduler_->ShouldFireTurbo()) {
    command_ = game::Command(game::TurboToggle::kToggleOn);
  } else if (switch_scheduler_->ShouldSwitch()) {
    command_ = game::Command(switch_scheduler_->SwitchDirection());
  } else {
    command_ = game::Command(throttle_scheduler_->throttle());
  }
}

void BulkScheduler::set_strategy(const Strategy& strategy) {
  strategy_ = strategy;
  throttle_scheduler_->set_strategy(strategy);
  switch_scheduler_->set_strategy(strategy);
  turbo_scheduler_->set_strategy(strategy);
}


void BulkScheduler::Overtake(const string& color) {
  printf("NOT IMPLEMENTED\n");
}

void BulkScheduler::IssuedCommand(const game::Command& command) {
  if (command.SwitchSet()) {
    printf("Switch\n");
    switch_scheduler_->Switched();
  } else if (command.TurboSet()) {
    printf("YABADABADUUUU\n");
    turbo_scheduler_->TurboUsed();
  }
}

}  // namespace schedulers
