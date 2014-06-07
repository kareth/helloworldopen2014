#include "schedulers/bulk_scheduler.h"
#include "schedulers/always_switch_scheduler.h"
#include "schedulers/never_switch_scheduler.h"
#include "schedulers/binary_throttle_scheduler.h"
#include "schedulers/wojtek_throttle_scheduler.h"
#include "utils/deadline.h"

#include "gflags/gflags.h"
#include <assert.h>

DECLARE_bool(check_if_safe_ahead);

DECLARE_string(throttle_scheduler);
DECLARE_string(switch_scheduler);
DECLARE_bool(disable_attack);

namespace schedulers {

BulkScheduler::BulkScheduler(const game::Race& race,
               game::RaceTracker& race_tracker,
               game::CarTracker& car_tracker)
    : race_(race), car_tracker_(car_tracker), race_tracker_(race_tracker) {
  turbo_scheduler_.reset(new GreedyTurboScheduler(race_, car_tracker_));
  throttle_scheduler_.reset(CreateThrottleScheduler());
  switch_scheduler_.reset(CreateSwitchScheduler());

  bump_scheduler_.reset(
      new BumpScheduler(race_, race_tracker_, car_tracker_));

  last_throttle_ = 0;
}

void BulkScheduler::Schedule(const game::CarState& state, int game_tick, const utils::Deadline& deadline) {
  if (!FLAGS_disable_attack) {
    bump_scheduler_->Schedule(state);

    if (bump_scheduler_->HasTarget()) {
      command_ = bump_scheduler_->command();
      if (!command_.SwitchSet() && !command_.TurboSet())
        last_throttle_ = command_.throttle();
      return;
    }
  }

  // Bumper has priority over anything else.

  turbo_scheduler_->Schedule(state);
  switch_scheduler_->Schedule(state);


  //std::cout << state.position().ShortDebugString() << std::endl;
  //printf("(%d %lf %lf)\n", switch_scheduler_->ExpectedSwitch(), switch_scheduler_->DistanceToSwitch(), last_throttle_);
  //if (switch_scheduler_->DistanceToSwitch() > 1000)
  //  assert(false);
  //printf("\n");
  // We assume that the path wih given switch is safe!
  auto state_with_switch = state;
  if (switch_scheduler_->ExpectedSwitch() != game::Switch::kStay)
    state_with_switch.set_switch_state(switch_scheduler_->ExpectedSwitch());
  throttle_scheduler_->Schedule(state_with_switch,
                                game_tick,
                                deadline,
                                switch_scheduler_->DistanceToSwitch(),
                                last_throttle_);
  // I assume that after throttle_scheduler, there is nothing computationally intensive, so that throttle_scheduler can take all remaining time (deadline)

  if (turbo_scheduler_->ShouldFireTurbo()) {
    command_ = game::Command(game::TurboToggle::kToggleOn);
  } else if (throttle_scheduler_->TimeToSwitch(game_tick)) {
    command_ = game::Command(switch_scheduler_->SwitchDirection());
  } else {
    command_ = game::Command(throttle_scheduler_->throttle());
  }

  game::Command safe_command;
  if (FLAGS_check_if_safe_ahead) {
    // Make sure that if we want to make switch now, we don't use state_with_switch.
    // That could cause us to ignore the switch even though we haven't actually switched.
    const game::CarState* s = command_.SwitchSet() ? &state : &state_with_switch;
    if (!race_tracker_.IsSafeAhead(*s, command_, &safe_command)) {
      std::cout << "INFO: It is not safe ahead. Slowing down." << std::endl;
      command_ = safe_command;
    }
  }

  if (!command_.SwitchSet() && !command_.TurboSet())
    last_throttle_ = command_.throttle();
  switch_scheduler_->set_last_throttle(last_throttle_);
  //std::cout << command_.DebugString() << std::endl;
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
    printf("Switch (%d)\n", int(command.get_switch()));
    switch_scheduler_->Switched();
  } else if (command.TurboSet()) {
    printf("YABADABADUUUU\n");
    turbo_scheduler_->TurboUsed();
  }
}

ThrottleScheduler* BulkScheduler::CreateThrottleScheduler() {
  if (FLAGS_throttle_scheduler == "BinaryThrottleScheduler") {
    std::cout << "Using BinaryThrottleScheduler" << std::endl;
    return new BinaryThrottleScheduler(race_, car_tracker_);
  } else if (FLAGS_throttle_scheduler == "WojtekThrottleScheduler") {
    std::cout << "Using WojtekThrottleScheduler" << std::endl;
    return new WojtekThrottleScheduler(race_, car_tracker_);
  }

  std::cerr << "UNKNOWN throttle scheduler: " << FLAGS_throttle_scheduler << std::endl;
  return new WojtekThrottleScheduler(race_, car_tracker_);
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
