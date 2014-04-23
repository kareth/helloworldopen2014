#ifndef CPP_SCHEDULERS_SHORTEST_PATH_SWITCH_SCHEDULER_H_
#define CPP_SCHEDULERS_SHORTEST_PATH_SWITCH_SCHEDULER_H_

#include "schedulers/switch_scheduler.h"

namespace schedulers {

class ShortestPathSwitchScheduler : public SwitchScheduler {
 public:
   ShortestPathSwitchScheduler(const game::Race& race,
                        game::CarTracker& car_tracker);
   // Returns scheduled switch
   // TODO(kareth) add some kind of importancy
   // e.g max if its last moment to switch etc
   bool ShouldSwitch();
   game::Switch SwitchDirection();

   // Prepares for overtake
   void Overtake(const string& color);

   // Sets lap strategy
   void set_strategy(const Strategy& strategy) { strategy_ = strategy; }

   // Updates the state and calculates next state
   void Schedule(const game::CarState& state);

   void Switched();

 private:
   void ComputeShortestPaths();

   // TODO remove
   int NextSwitch(int piece_index);
   double LaneLength(const game::Position& position, int lane, int from, int to);

   game::CarTracker& car_tracker_;
   const game::Race& race_;
   Strategy strategy_;

   bool should_switch_now_;
   game::Switch scheduled_switch_;
   int target_switch_;
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_SHORTEST_PATH_SWITCH_SCHEDULER_H_
