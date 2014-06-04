#include "schedulers/bulk_scheduler.h"
#include "schedulers/always_switch_scheduler.h"
#include "schedulers/never_switch_scheduler.h"
#include "schedulers/binary_throttle_scheduler.h"
#include "schedulers/wojtek_throttle_scheduler.h"
#include "schedulers/magic_throttle_scheduler.h"

#include "gflags/gflags.h"

DECLARE_bool(check_if_safe_ahead);

DECLARE_string(throttle_scheduler);
DECLARE_string(switch_scheduler);

namespace schedulers {

BulkScheduler::BulkScheduler(const game::Race& race,
               game::RaceTracker& race_tracker,
               game::CarTracker& car_tracker,
               int time_limit)
    : race_(race), car_tracker_(car_tracker), race_tracker_(race_tracker), time_limit_(time_limit) {
  turbo_scheduler_.reset(new GreedyTurboScheduler(race_, car_tracker_));
  throttle_scheduler_.reset(CreateThrottleScheduler());
  switch_scheduler_.reset(CreateSwitchScheduler());

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
  if (switch_scheduler_->ExpectedSwitch() != game::Switch::kStay)
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

ThrottleScheduler* BulkScheduler::CreateThrottleScheduler() {
  if (FLAGS_throttle_scheduler == "BinaryThrottleScheduler") {
    std::cout << "Using BinaryThrottleScheduler" << std::endl;
    return new BinaryThrottleScheduler(race_, car_tracker_, time_limit_);
  } else if (FLAGS_throttle_scheduler == "WojtekThrottleScheduler") {
    std::cout << "Using WojtekThrottleScheduler" << std::endl;
    return new WojtekThrottleScheduler(race_, car_tracker_, time_limit_);
  } else if (FLAGS_throttle_scheduler == "MagicThrottleScheduler") {
    std::cout << "Using " << FLAGS_throttle_scheduler << std::endl;
    return new MagicThrottleScheduler(race_, car_tracker_, time_limit_);
  }

  std::cerr << "UNKNOWN throttle scheduler: " << FLAGS_throttle_scheduler << std::endl;
  return new BinaryThrottleScheduler(race_, car_tracker_, time_limit_);
}

SwitchScheduler* BulkScheduler::CreateSwitchScheduler() {
  if (FLAGS_switch_scheduler == "ShortestPathSwitchScheduler") {
    std::cout << "Using ShortestPathSwitchScheduler" << std::endl;
    return new ShortestPathSwitchScheduler(race_, race_tracker_, car_tracker_);
  } else if (FLAGS_switch_scheduler == "AlwaysSwitchScheduler") {
    std::cout << "Using AlwaysSwitchScheduler" << std::endl;
    return new AlwaysSwitchScheduler(&race_.track());
  } else if (FLAGS_switch_scheduler == "NeverSwitchScheduler") {
    std::cout << "Using NeverSwitchScheduler" << std::endl;
    return new NeverSwitchScheduler();
  }

  std::cerr << "UNKNOWN switch scheduler: " << FLAGS_switch_scheduler << std::endl;
  return new ShortestPathSwitchScheduler(race_, race_tracker_, car_tracker_);
}

}  // namespace schedulers
