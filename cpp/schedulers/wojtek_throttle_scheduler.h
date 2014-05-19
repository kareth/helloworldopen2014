#ifndef CPP_SCHEDULERS_WOJTEK_THROTTLE_SCHEDULER_H_
#define CPP_SCHEDULERS_WOJTEK_THROTTLE_SCHEDULER_H_

#include <vector>
#include "game/car_tracker.h"
#include "schedulers/strategy.h"
#include "schedulers/throttle_scheduler.h"

namespace schedulers {

class WojtekThrottleScheduler : public ThrottleScheduler {
 public:
  // Expected time limit in miliseconds
  WojtekThrottleScheduler(const game::Race* race,
                          const game::CarTracker* car_tracker);

  // Returns scheduled throttle
  double throttle() override { return throttle_; };

  // Prepares for overtake
  void Overtake(const string& color) override { }

  // Sets lap speed strategy
  void set_strategy(const Strategy& strategy) override {  }

  // Updates the state and calculates next state
  void Schedule(const game::CarState& state) override;

  const std::vector<double>& full_schedule() const override { return throttles_; }

 private:
  const game::CarTracker* car_tracker_;
  const game::Race* race_;
  double throttle_ = 1.0;
  vector<double> throttles_;
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_WOJTEK_THROTTLE_SCHEDULER_H_
