#ifndef CPP_SCHEDULERS_SHORTEST_PATH_SWITCH_SCHEDULER_H_
#define CPP_SCHEDULERS_SHORTEST_PATH_SWITCH_SCHEDULER_H_

#include "schedulers/switch_scheduler.h"
#include "game/race_tracker.h"

DECLARE_bool(overtake);

namespace schedulers {

class ShortestPathSwitchScheduler : public SwitchScheduler {
 public:
   ShortestPathSwitchScheduler(const game::Race& race,
                        game::RaceTracker& race_tracker,
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
   bool IsLaneSafe(const game::CarState& state, int from, int to, int lane);

   double LaneLength(const game::Position& position, int lane, int from, int to);

   const static int kInf = 1000000000;

   game::CarTracker& car_tracker_;
   game::RaceTracker& race_tracker_;
   const game::Race& race_;
   Strategy strategy_;

   bool should_switch_now_;
   bool waiting_for_switch_;
   game::Switch scheduled_switch_;
   int target_switch_;
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_SHORTEST_PATH_SWITCH_SCHEDULER_H_
