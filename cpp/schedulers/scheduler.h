#ifndef SCHEDULERS_SCHEDULER_H_
#define SCHEDULERS_SCHEDULER_H_

#include <string>
#include "game/command.h"
#include "game/car_tracker.h"
#include "schedulers/strategy.h"

namespace schedulers {

class Scheduler {
 public:
   virtual void Schedule(const game::CarState& state) = 0;

   virtual void Overtake(const string& color) = 0;

   virtual void set_strategy(const Strategy& strategy) = 0;

   virtual game::Command command() = 0;

   virtual void IssuedCommand(const game::Command& command) = 0;

 private:

};

}  // namespace schedulers

#endif  // SCHEDULERS_SCHEDULER_H_
