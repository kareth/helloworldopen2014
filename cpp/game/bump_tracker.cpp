#include "game/bump_tracker.h"

namespace game {

BumpTracker::BumpTracker(game::CarTracker& car_tracker,
    const game::Race& race)
  : race_(race), car_tracker_(car_tracker) {
  // TODO hardcoded time limit
  // TODO(kareth): This is too slow! Use WojtekThrottleScheduler instead, maybe with some quick groups.
  throttle_scheduler_.reset(
      new schedulers::BinaryThrottleScheduler(
        race_, car_tracker_, {1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3}));
}

bool BumpTracker::CanBump(const CarState& bumping_state, const CarState& bumped_state) {
  return CanBumpOptimalEnemy(bumping_state, bumped_state);
}

bool BumpTracker::CanBumpOptimalEnemy(const CarState& bumping_state, const CarState& bumped_state) {
  if (race_.track().IsFirstInFront(bumping_state.position(), bumped_state.position()))
    return false;

  //TODO(kareth?) FIXME: I need actuall deadline here
  throttle_scheduler_->Schedule(bumped_state, 0, utils::Deadline());
  auto& schedule = throttle_scheduler_->full_schedule();

  auto bumping = bumping_state;
  auto bumped = bumped_state;

  for (double bumped_throttle : schedule) {
    bumping = car_tracker_.Predict(bumping, Command(1));
    bumped = car_tracker_.Predict(bumped, Command(bumped_throttle));

    if (!car_tracker_.crash_model().IsSafe(bumping.position().angle()))
      return false;

    if (race_.track().IsFirstInFront(bumping.position(), bumped.position()))
      return true;
  }
  return false;
}

bool BumpTracker::CanBumpForSure(const CarState& bumping_state, const CarState& bumped_state, int in_ticks) {
  auto bumping = bumping_state;
  auto bumped = bumped_state;

  for (int i = 0; i < in_ticks; i++) {
    bumping = car_tracker_.Predict(bumping, Command(1));
    bumped = car_tracker_.Predict(bumped, Command(1));

    double distance = car_tracker_.DistanceBetween(bumping.position(), bumped.position());

    if (!car_tracker_.crash_model().IsSafe(bumping.position().angle()) ||
        !car_tracker_.crash_model().IsSafe(bumped.position().angle()))
      return false;

    if (distance < race_.cars()[0].length()) {
      printf("CanBumpForSure in %d ticks\n", i);
      return true;
    }
  }
  return false;
}

bool BumpTracker::CanBumpWithTurbo(const CarState& bumping_state, const CarState& bumped_state, int in_ticks) {
  if (!bumping_state.turbo_state().available() && !bumping_state.turbo_state().is_on())
    return false;

  auto bumping = bumping_state;
  auto bumped = bumped_state;

  for (int i = 0; i < in_ticks - 1; i++) {
    if (i == 0 && bumping_state.turbo_state().available())
      bumping = car_tracker_.Predict(bumping, Command::Turbo());
    else
      bumping = car_tracker_.Predict(bumping, Command(1));
    bumped = car_tracker_.Predict(bumped, Command(1));

    double distance = car_tracker_.DistanceBetween(bumping.position(), bumped.position());

    if (!car_tracker_.crash_model().IsSafe(bumping.position().angle()) ||
        !car_tracker_.crash_model().IsSafe(bumped.position().angle()))
      return false;

    if (distance < race_.cars()[0].length()) {
      printf("CanBumpForSureWithTURBO in %d ticks\n", i);
      return true;
    }
  }
  return false;
}

}  // namespace game
