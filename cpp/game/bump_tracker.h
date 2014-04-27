#ifndef CPP_GAME_BUMP_TRACKER_H_
#define CPP_GAME_BUMP_TRACKER_H_

#include "game/race.h"
#include "game/car_tracker.h"
#include "schedulers/binary_throttle_scheduler.h"

namespace game {

class BumpTracker {
 public:
  BumpTracker(game::CarTracker& car_tracker, const game::Race& race);

  bool CanBump(const CarState& bumping, const CarState& bumped);

  // TODO private?
  // We go full 1
  // Enemy go optimal by binary scheduler
  bool CanBumpOptimalEnemy(const CarState& bumping, const CarState& bumped);

  // We go 1
  // Enemy go 1
  bool CanBumpForSure(const CarState& bumping, const CarState& bumped, int in_ticks = 50);

  // I use turbo and run all out
  // he go 1
  bool CanBumpWithTurbo(const CarState& bumping, const CarState& bumped, int in_ticks = 50);

 private:

  const game::Race& race_;
  CarTracker& car_tracker_;

  std::unique_ptr<schedulers::ThrottleScheduler> throttle_scheduler_;
};

}  // namespace game

#endif  // CPP_GAME_BUMP_TRACKER_H_
