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
   game::Switch ExpectedSwitch();

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

   // The distance between 'start' and 'end' assuming that 'end' position is on
   // lane 'lane'.
   //
   // If lane is invalid, returns kInf.
   double DistanceBetween(const game::Position& start, const game::Position& end, int lane);

   double SlowestCarOnLane(const game::CarState& state, int from, int to, int lane);

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
