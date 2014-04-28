#ifndef CPP_SCHEDULERS_BUMP_SCHEDULER_H_
#define CPP_SCHEDULERS_BUMP_SCHEDULER_H_

#include <string>
#include "game/command.h"
#include "game/car_tracker.h"
#include "game/race_tracker.h"
#include "schedulers/strategy.h"
#include "game/bump_tracker.h"

namespace schedulers {

class BumpScheduler {
 public:
  BumpScheduler(const game::Race& race,
               game::RaceTracker& race_tracker,
               game::CarTracker& car_tracker);

  void Schedule(const game::CarState& state);

  game::Command command() { return command_; }

  void set_strategy(const Strategy& strategy) {}

  bool HasTarget() const { return has_bump_target_; }

  void IssuedCommand(const game::Command& command) {}

  game::Command BumpCommand(const game::CarState& state);

 private:
  game::Command FollowSwitch(const game::CarState& me, const game::CarState& target);

  game::CarTracker& car_tracker_;
  game::RaceTracker& race_tracker_;
  const game::Race& race_;

  game::Command command_;

  std::string bump_target_;
  bool has_bump_target_ = false;

  std::unique_ptr<game::BumpTracker> bump_tracker_;
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_BUMP_SCHEDULER_H_
