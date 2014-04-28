#include "schedulers/bump_scheduler.h"

DECLARE_bool(check_if_safe_ahead);
DECLARE_bool(bump_with_turbo);

using game::Command;

namespace schedulers {

BumpScheduler::BumpScheduler(const game::Race& race,
             game::RaceTracker& race_tracker,
             game::CarTracker& car_tracker)
 : race_(race), car_tracker_(car_tracker), race_tracker_(race_tracker),
   has_bump_target_(false), bump_target_("") {
  bump_tracker_.reset(new game::BumpTracker(car_tracker_, race_));
}

void BumpScheduler::Schedule(const game::CarState& state) {
  if (has_bump_target_) {
    command_ = BumpCommand(state);
  } else {
    // Check if there is any1 to bump
    if (FLAGS_bump_with_turbo &&
        !state.turbo_state().available()) {

      // Check all cars
      for (auto& enemy : race_tracker_.enemies()) {
        Command safe_command;

        // TODO doesnt check if guy crashes
        if (bump_tracker_->CanBumpWithTurbo(state, enemy.state()) &&
            race_tracker_.WorthBumping(enemy.color()) &&
            race_tracker_.IsSafeAttack(Command::Turbo(), &safe_command)) {
          has_bump_target_ = true;
          bump_target_ = enemy.color();
          command_ = Command::Turbo();
          return;
        }
      }
    }
  }
}

game::Command BumpScheduler::BumpCommand(const game::CarState& state) {
  if (race_tracker_.enemy(bump_target_).state().position().end_lane() !=
      state.position().end_lane()) {
    return FollowSwitch(state, race_tracker_.enemy(bump_target_).state());
  }
  Command safe_command;

  for (double throttle = 1; throttle >= 0; throttle -= 0.2)
    if (race_tracker_.IsSafeAttack(Command(throttle), &safe_command))
      return Command(throttle);

  if (race_tracker_.IsSafeInFront(Command(1), &safe_command))
    return Command(1);
  return Command(0);
}

game::Command BumpScheduler::FollowSwitch(const game::CarState& me, const game::CarState& target) {
  if (me.position().end_lane() - 1 == target.position().end_lane())
    return Command(game::Switch::kSwitchLeft);
  if (me.position().end_lane() + 1 == target.position().end_lane())
    return Command(game::Switch::kSwitchRight);
  return Command(1);
}

}  // namespace schedulers
