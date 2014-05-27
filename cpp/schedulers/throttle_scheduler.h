#ifndef CPP_SCHEDULERS_THROTTLE_SCHEDULER_H_
#define CPP_SCHEDULERS_THROTTLE_SCHEDULER_H_

#include "schedulers/strategy.h"
#include "game/car_predictor.h"

namespace schedulers {

class ThrottleScheduler {
 public:
  // Returns scheduled throttle
  virtual double throttle() = 0;

  // Prepares for overtake
  virtual void Overtake(const string& color) = 0;

  // Sets lap speed strategy
  virtual void set_strategy(const Strategy& strategy) = 0;

  // Updates the state and calculates next state
  virtual void Schedule(const game::CarState& state) = 0;

  virtual const std::vector<double>& full_schedule() const = 0;
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_THROTTLE_SCHEDULER_H_
