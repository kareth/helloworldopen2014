#include "schedulers/bump_scheduler.h"

DECLARE_bool(check_if_safe_ahead);
DECLARE_bool(bump_with_turbo);
DEFINE_bool(new_attack, false, "");

using game::Command;
using ::game::CarState;

namespace schedulers {

BumpScheduler::BumpScheduler(const game::Race& race,
             game::RaceTracker& race_tracker,
             game::CarTracker& car_tracker)
 : race_(race), car_tracker_(car_tracker), race_tracker_(race_tracker),
   has_bump_target_(false), bump_target_("") {
  bump_tracker_.reset(new game::BumpTracker(car_tracker_, race_));
}

void BumpScheduler::Schedule(const game::CarState& state) {
  if (FLAGS_new_attack) {
    Schedule2(state);
    return;
  }
  if (has_bump_target_) {
    // Already bumped
    if (race_tracker_.BumpOccured(race_tracker_.my_color(), bump_target_)) {
      has_bump_target_ = false;
      // Cant reach anymore
    } else if (!bump_tracker_->CanBumpForSure(state, race_tracker_.enemy(bump_target_).state())) {

      // If we changed lane, check what happens if we change lane ^^
      if (state.position().end_lane() != race_tracker_.enemy(bump_target_).state().position().end_lane()) {
        auto me = car_tracker_.Predict(state, FollowSwitch(state, race_tracker_.enemy(bump_target_).state()));
        auto target = car_tracker_.Predict(race_tracker_.enemy(bump_target_).state(), Command(1));

        if (!bump_tracker_->CanBumpForSure(me, target))
          has_bump_target_ = false;
        else
          command_ = BumpCommand(state);
      } else {
        has_bump_target_ = false;
      }

    } else {
      command_ = BumpCommand(state);
    }
  } else {

    // No turbo check
    // Check all cars
    for (auto& enemy : race_tracker_.enemies()) {
      if (enemy.color() == race_tracker_.my_color())
        continue;

      Command safe_command;
      // TODO doesnt check if guy crashes
      if (bump_tracker_->CanBumpForSure(state, enemy.state()) &&
          !enemy.is_dead() &&
          race_tracker_.IsSafeAttack(state, Command(1), &safe_command)) {
        printf("Safe bump coming! Target: %s\n", enemy.color().c_str());
        has_bump_target_ = true;
        bump_target_ = enemy.color();
        command_ = Command(1);
        return;
      }
    }

    // Turboo bump
    if (FLAGS_bump_with_turbo &&
        state.turbo_state().available()) {

      // Check all cars
      for (auto& enemy : race_tracker_.enemies()) {
        if (enemy.color() == race_tracker_.my_color())
          continue;

        Command safe_command;
        // TODO doesnt check if guy crashes
        if (race_tracker_.WorthBumping(enemy.color()) &&
            bump_tracker_->CanBumpWithTurbo(state, enemy.state()) &&
            race_tracker_.IsSafeAttack(state, Command::Turbo(), &safe_command)) {
          printf("Tuuuurbooo bump! Dieeeeeee ;P\n");
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
    if (race_tracker_.IsSafeAttack(state, Command(throttle), &safe_command))
      return Command(throttle);

  if (race_tracker_.IsSafeAhead(state, Command(1), &safe_command))
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

void BumpScheduler::Schedule2(const CarState& state) {

}

}  // namespace schedulers
