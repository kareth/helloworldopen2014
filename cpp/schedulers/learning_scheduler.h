#ifndef SCHEDULERS_LEARNING_SCHEDULER_H_
#define SCHEDULERS_LEARNING_SCHEDULER_H_

#include <string>
#include "game/command.h"
#include "game/car_tracker.h"
#include "schedulers/strategy.h"
#include "schedulers/scheduler.h"

#include "schedulers/binary_throttle_scheduler.h"

namespace schedulers {

class LearningScheduler : public Scheduler {
 public:
   LearningScheduler(const game::Race& race,
                 game::CarTracker& car_tracker,
                 int time_limit);

   void Schedule(const game::CarState& state) override;

   void Overtake(const string& color) override;

   game::Command command() override { return command_; }

   void set_strategy(const Strategy& strategy) override;

   void IssuedCommand(const game::Command& command) override;

  private:
   Strategy strategy_;
   game::Command command_;

   game::CarTracker& car_tracker_;
   const game::Race& race_;

   std::unique_ptr<schedulers::ThrottleScheduler> throttle_scheduler_;
};

}  // namespace schedulers

#endif  // SCHEDULERS_LEARNING_SCHEDULER_H_
