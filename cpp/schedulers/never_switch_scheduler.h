#ifndef CPP_SCHEDULERS_NEVER_SWITCH_SCHEDULER_H_
#define CPP_SCHEDULERS_NEVER_SWITCH_SCHEDULER_H_

#include "game/race_tracker.h"
#include "game/track.h"
#include "schedulers/switch_scheduler.h"

namespace schedulers {

class NeverSwitchScheduler : public SwitchScheduler {
 public:
  NeverSwitchScheduler() { }

  bool ShouldSwitch() override {
    return false;
  }

  game::Switch SwitchDirection() override {
    return switch_;
  }

  game::Switch ExpectedSwitch() override {
    return switch_;
  }

  // Prepares for overtake
  void Overtake(const string& color) override { }

  // Sets lap strategy
  void set_strategy(const Strategy& strategy) override { }

  // Updates the state and calculates next state
  void Schedule(const game::CarState& state) override {
  }

  void Switched() override {
  }

 private:
  game::Switch switch_ = game::Switch::kStay;
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_NEVER_SWITCH_SCHEDULER_H_
