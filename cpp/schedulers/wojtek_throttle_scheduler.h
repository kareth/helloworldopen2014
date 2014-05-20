#ifndef CPP_SCHEDULERS_WOJTEK_THROTTLE_SCHEDULER_H_
#define CPP_SCHEDULERS_WOJTEK_THROTTLE_SCHEDULER_H_

#include <vector>
#include <array>
#include <cmath>
#include <random>
#include "game/car_tracker.h"
#include "schedulers/strategy.h"
#include "schedulers/throttle_scheduler.h"

namespace schedulers {

class Sched {
 public:
  Sched(int horizon) : throttles(horizon) {}
  vector<double> throttles;
  double distance = 0;
  size_t size() const { return throttles.size(); }
};

class WojtekThrottleScheduler : public ThrottleScheduler {
 public:
  static const int HORIZON;
  static const vector<double> values; // possible throttle values to check

  // Expected time limit in miliseconds
  WojtekThrottleScheduler(const game::Race* race,
                          game::CarTracker* car_tracker);

  // Returns scheduled throttle
  double throttle() override { return throttle_; };

  // Prepares for overtake
  void Overtake(const string& color) override { }

  // Sets lap speed strategy
  void set_strategy(const Strategy& strategy) override {  }

  // Updates the state and calculates next state
  void Schedule(const game::CarState& state) override;

  const std::vector<double>& full_schedule() const override { return best_schedule_.throttles; }

  // Updates throttles_. Return distance
  bool Optimize(const game::CarState& state);

  bool IsSafe(const game::CarState& state, const class Sched& schedule);

  game::CarState Predict(const game::CarState& state, const class Sched& schedule);

  double Distance(const game::CarState& state, const class Sched& schedule);

  bool Check(const game::CarState& state, int from, Sched& schedule);

  bool VNS(const game::CarState& state, Sched& schedule);

  bool Improve(const game::CarState& state, Sched& schedule, int step);

  bool ImproveOne(const game::CarState& state, Sched& schedule, int idx, double step);

 private:
  game::CarTracker* car_tracker_;
  const game::Race* race_;
  double throttle_ = 1.0;
  Sched best_schedule_;
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_WOJTEK_THROTTLE_SCHEDULER_H_
