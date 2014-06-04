#ifndef CPP_SCHEDULERS_MAGIC_THROTTLE_SCHEDULER_H_
#define CPP_SCHEDULERS_MAGIC_THROTTLE_SCHEDULER_H_

#include <vector>
#include <array>
#include <cmath>
#include <random>
#include "game/car_tracker.h"
#include "schedulers/strategy.h"
#include "schedulers/throttle_scheduler.h"
#include "schedulers/schedule.h"
#include "schedulers/bb.h"

namespace schedulers {

class MagicThrottleScheduler : public ThrottleScheduler {
 public:
  static const int HORIZON;
  static const int N;

  // Expected time limit in miliseconds
  MagicThrottleScheduler(const game::Race& race,
                         game::CarTracker& car_tracker, int time_limit);

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
  game::CarTracker& car_tracker_;
  const game::Race& race_;
  const int time_limit_;
  Sched best_schedule_;

  int tick_ = 0; //TODO: This is bad here. Should be somewhere globally

  void Log(const game::CarState& state);
  void ImproveByMagic(const game::CarState& state, Sched& schedule);
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_MAGIC_THROTTLE_SCHEDULER_H_

