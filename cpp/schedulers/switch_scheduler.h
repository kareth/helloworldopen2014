#ifndef CPP_SCHEDULERS_SWITCH_SCHEDULER_H_
#define CPP_SCHEDULERS_SWITCH_SCHEDULER_H_

#include "game/car_tracker.h"
#include "schedulers/strategy.h"

namespace schedulers {

class SwitchScheduler {
 public:
   // Returns scheduled throttle
   // TODO(kareth) add some kind of importancy
   // e.g max if its last moment to switch etc
   virtual bool ShouldSwitch() = 0;
   virtual game::Switch SwitchDirection() = 0;
   virtual game::Switch ExpectedSwitch() = 0;

   // Prepares for overtake
   virtual void Overtake(const string& color) = 0;

   // Sets lap speed strategy
   virtual void set_strategy(const Strategy& strategy) = 0;

   // Updates the state and calculates next state
   virtual void Schedule(const game::CarState& state) = 0;

   virtual void Switched() = 0;

};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_SWITCH_SCHEDULER_H_
