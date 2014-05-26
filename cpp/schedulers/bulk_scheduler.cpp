#include "schedulers/bulk_scheduler.h"
#include "schedulers/always_switch_scheduler.h"

#include "gflags/gflags.h"

DECLARE_bool(check_if_safe_ahead);
DEFINE_bool(always_switch, false, "");

namespace schedulers {

BulkScheduler::BulkScheduler(const game::Race& race,
               game::RaceTracker& race_tracker,
               game::CarTracker& car_tracker,
               int time_limit)
 : race_(race), car_tracker_(car_tracker), race_tracker_(race_tracker) {
  throttle_scheduler_.reset(
      //new BinaryThrottleScheduler(race_, car_tracker_, time_limit));
      new WojtekThrottleScheduler(&race_, &car_tracker_));
  turbo_scheduler_.reset(
      new GreedyTurboScheduler(race_, car_tracker_));
  if (FLAGS_always_switch) {
    switch_scheduler_.reset(new AlwaysSwitchScheduler(&race_.track()));
  } else {
    switch_scheduler_.reset(
        new ShortestPathSwitchScheduler(race_, race_tracker_, car_tracker_));
  }
  bump_scheduler_.reset(
      new BumpScheduler(race_, race_tracker_, car_tracker_));
}

void BulkScheduler::Schedule(const game::CarState& state) {
  bump_scheduler_->Schedule(state);

  if (bump_scheduler_->HasTarget()) {
    command_ = bump_scheduler_->command();
    return;
  }
  // Bumper has priority over anything else.

  turbo_scheduler_->Schedule(state);
  switch_scheduler_->Schedule(state);

  // If we want to switch, schedule throttle for target lane bent
  auto state_with_switch = state;
  state_with_switch.set_switch_state(switch_scheduler_->ExpectedSwitch());
  throttle_scheduler_->Schedule(state_with_switch);

  if (turbo_scheduler_->ShouldFireTurbo()) {
    command_ = game::Command(game::TurboToggle::kToggleOn);
  } else if (switch_scheduler_->ShouldSwitch()) {
    command_ = game::Command(switch_scheduler_->SwitchDirection());
  } else {
    command_ = game::Command(throttle_scheduler_->throttle());
  }

  game::Command safe_command;
  if (FLAGS_check_if_safe_ahead &&
      !race_tracker_.IsSafeInFront(command_, &safe_command))
    command_ = safe_command;
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
