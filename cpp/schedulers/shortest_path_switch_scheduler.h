#ifndef CPP_SCHEDULERS_SHORTEST_PATH_SWITCH_SCHEDULER_H_
#define CPP_SCHEDULERS_SHORTEST_PATH_SWITCH_SCHEDULER_H_

#include "schedulers/switch_scheduler.h"
#include "game/race_tracker.h"
#include "game/double_path_optimizer.h"

DECLARE_bool(overtake);

namespace schedulers {

class ShortestPathSwitchScheduler : public SwitchScheduler {
 public:
   ShortestPathSwitchScheduler(const game::Race& race,
                        game::RaceTracker& race_tracker,
                        game::CarTracker& car_tracker);

   // Calculates the most optimal switch decision
   // based on lengths, and cars on track
   void Schedule(const game::CarState& state);

   // Saves that switch command has been sent this tick
   // IMPORTANT: Must allways be invoked once we switch
   void Switched();

   // Returns true if we are less than 2 ticks befor the switch
   // So there are just 2 possible moments where it will return true
   bool ShouldSwitch();

   // Returns switch direction of already scheduled switch
   // Should only be used to issue command
   // if ShouldSwitch() equals true
   game::Switch SwitchDirection() { return direction_; };

   // Returns a command that we plane to issue soon (just before switch)
   // If we dont plan to switch on next switch it returns Switch::kStay
   //
   // Note: Be careful while using it to hardchange carState switch_state
   // as you can override the Switch command that u sent earlier with kStay
   game::Switch ExpectedSwitch();

   // Sets lap strategy
   void set_strategy(const Strategy& strategy) { strategy_ = strategy; }

 private:
  bool WaitingToReachIssuedSwitch(const game::CarState& state);

   // True if we plan to issue command before next switch
   bool should_switch_now_;

   // True if we already issued switch command and wait to reach switch
   bool waiting_for_switch_;

   // Index of piece where we want to switch
   int target_switch_;

   // Scheduled direction
   game::Switch direction_;

   std::unique_ptr<game::PathOptimizerInterface> path_optimizer_;

   game::CarTracker& car_tracker_;
   game::RaceTracker& race_tracker_;
   const game::Race& race_;
   Strategy strategy_;
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_SHORTEST_PATH_SWITCH_SCHEDULER_H_
