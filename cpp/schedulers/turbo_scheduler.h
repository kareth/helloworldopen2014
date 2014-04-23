#ifndef CPP_SCHEDULERS_TURBO_SCHEDULER_H_
#define CPP_SCHEDULERS_TURBO_SCHEDULER_H_

#include "schedulers/strategy.h"
#include "game/turbo.h"
#include "game/car_tracker.h"

namespace schedulers {

class TurboScheduler {
 public:
   // Returns if should we use turbo
   virtual bool ShouldFireTurbo() = 0;

   // Makes decision on turbo usage
   virtual void Schedule(const game::CarState& state) = 0;

   // Prepare for overtake
   virtual void Overtake(const string& color) = 0;

   virtual void set_strategy(const Strategy& strategy) = 0;

   virtual void NewTurbo(const game::Turbo& turbo) = 0;

   virtual void TurboUsed() = 0;
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_TURBO_SCHEDULER_H_
