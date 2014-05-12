#ifndef CPP_SCHEDULERS_ALWAYS_SWITCH_SCHEDULER_H_
#define CPP_SCHEDULERS_ALWAYS_SWITCH_SCHEDULER_H_

#include "game/race_tracker.h"
#include "game/track.h"
#include "schedulers/switch_scheduler.h"

DECLARE_bool(overtake);

namespace schedulers {

class AlwaysSwitchScheduler : public SwitchScheduler {
 public:
   AlwaysSwitchScheduler(const game::Track* track) : track_(track) { }

   bool ShouldSwitch() override {
     return switch_ != game::Switch::kStay;
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
     if (state.switch_state() == game::Switch::kStay) {
       if (state.position().end_lane() != 0) {
         switch_ = game::Switch::kSwitchLeft;
       } else {
         switch_ = game::Switch::kSwitchRight;
       }
     } else {
       switch_ = game::Switch::kStay;
     }
   }

   void Switched() override {
   }

 private:
  const game::Track* track_;

  game::Switch switch_ = game::Switch::kStay;
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_ALWAYS_SWITCH_SCHEDULER_H_
