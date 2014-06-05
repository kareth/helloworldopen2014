#ifndef CPP_SCHEDULERS_WOJTEK_THROTTLE_SCHEDULER_H_
#define CPP_SCHEDULERS_WOJTEK_THROTTLE_SCHEDULER_H_

#include <vector>
#include <array>
#include <cmath>
#include <random>
#include <fstream>

#include "game/car_tracker.h"
#include "schedulers/strategy.h"
#include "schedulers/throttle_scheduler.h"
#include "schedulers/schedule.h"
#include "schedulers/bb.h"
#include "schedulers/local_improver.h"
#include "utils/deadline.h"

namespace schedulers {

class WojtekThrottleScheduler : public ThrottleScheduler {
 public:
  static const vector<int> DEFAULT_GROUPS;
  static const vector<int> QUICK_GROUPS;

  static const int HORIZON;
  static const vector<double> values; // possible throttle values to check

  // Expected time limit in miliseconds
  WojtekThrottleScheduler(const game::Race& race,
                          game::CarTracker& car_tracker, int time_limit,
                          const vector<int>& groups = DEFAULT_GROUPS);

  ~WojtekThrottleScheduler() override;

  // Returns scheduled throttle
  double throttle() override { return best_schedule_[0]; };

  // Prepares for overtake
  void Overtake(const string& color) override { }

  // Sets lap speed strategy
  void set_strategy(const Strategy& strategy) override {  }

  // Updates the state and calculates next state
  void Schedule(const game::CarState& state, int game_tick, 
                const utils::Deadline& deadline) override;

  const std::vector<double>& full_schedule() const override { return best_schedule_.throttles_; }

 private:
  void Log(const game::CarState& state);
  void PrintSchedule(const game::CarState& state, const Sched& schedule, int len);
  int horizon() const { return horizon_; }

  const game::Race& race_;
  game::CarTracker& car_tracker_;

  const vector<int> groups_;
  const int horizon_;

  Sched best_schedule_;

  BranchAndBound branch_and_bound_;
  LocalImprover local_improver_;
 
  // Some stats
  bool initial_schedule_safe_;
  double last_schedule_time_; // ms
  int last_game_tick_ = -1000;

  std::ofstream log_file_;
  int time_limit_; // ms
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_WOJTEK_THROTTLE_SCHEDULER_H_
