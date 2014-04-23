#ifndef CPP_SCHEDULERS_SHORTEST_PATH_SWITCH_SCHEDULER_H_
#define CPP_SCHEDULERS_SHORTEST_PATH_SWITCH_SCHEDULER_H_

namespace schedulers {

class ShortestPathSwitchScheduler : public SwitchScheduler {
 public:
   ShortestPathSwitchScheduler(const game::Race& race,
                        game::CarTracker& car_tracker);
   // Returns scheduled throttle
   // TODO(kareth) add some kind of importancy
   // e.g max if its last moment to switch etc
   bool ShouldSwitch();

   // Prepares for overtake
   void Overtake(const string& color);

   // Sets lap strategy
   void set_strategy(const Strategy& strategy) { strategy_ = strategy; }

   // Updates the state and calculates next state
   void Schedule(const game::CarState& state);

 private:
   void ComputeShortestPaths();

   game::CarTracker& car_tracker_;
   const game::Race& race_;
   Strategy strategy_;

   bool should_switch_now_;
   game::Switch schedule_;
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_SHORTEST_PATH_SWITCH_SCHEDULER_H_
