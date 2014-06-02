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

namespace schedulers {

class WojtekThrottleScheduler : public ThrottleScheduler {
 public:
  static const int HORIZON;
  static const vector<double> values; // possible throttle values to check

  // Expected time limit in miliseconds
  WojtekThrottleScheduler(const game::Race& race,
                          game::CarTracker& car_tracker, int time_limit);

  ~WojtekThrottleScheduler() override;

  // Returns scheduled throttle
  double throttle() override { return best_schedule_[0]; };

  // Prepares for overtake
  void Overtake(const string& color) override { }

  // Sets lap speed strategy
  void set_strategy(const Strategy& strategy) override {  }

  // Updates the state and calculates next state
  void Schedule(const game::CarState& state) override;

  const std::vector<double>& full_schedule() const override { return best_schedule_.throttles_; }

 private:
  bool Improve(const game::CarState& state, Sched& schedule, double step);
  bool ImproveOne(const game::CarState& state, Sched& schedule, int idx, double step);
  void Log(const game::CarState& state);
  void PrintSchedule(const game::CarState& state, const Sched& schedule, int len);

  game::CarTracker& car_tracker_;
  const game::Race& race_;
  Sched best_schedule_;
  BranchAndBound bb_;
  std::ofstream log_file_;
  double last_schedule_time_; // ms
  int tick_;
  int time_limit_; // ms
  bool initial_schedule_safe_; 
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_WOJTEK_THROTTLE_SCHEDULER_H_
