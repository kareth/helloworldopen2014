#ifndef CPP_SCHEDULERS_BINARY_THROTTLE_SCHEDULER_H_
#define CPP_SCHEDULERS_BINARY_THROTTLE_SCHEDULER_H_

#include <vector>
#include "game/car_tracker.h"
#include "schedulers/strategy.h"
#include "schedulers/throttle_scheduler.h"

namespace schedulers {

class BinaryThrottleScheduler : public ThrottleScheduler {
 public:
  // Expected time limit in miliseconds
  BinaryThrottleScheduler(const game::Race& race,
                          game::CarTracker& car_tracker,
                          int time_limit);

  BinaryThrottleScheduler(const game::Race& race,
                          game::CarTracker& car_tracker,
                          const std::vector<int>& groups);

  // Returns scheduled throttle
  double throttle() override;

  // Prepares for overtake
  void Overtake(const string& color) override;

  // Sets lap speed strategy
  void set_strategy(const Strategy& strategy) override { strategy_ = strategy; }

  // Updates the state and calculates next state
  void Schedule(const game::CarState& state) override;

 private:
  static const bool kLoggerOn = true;

  // Optimizes next steps to maximize distance.
  // Updates schedule_
  void Optimize(const game::CarState& state);

  // Finds most optimal(by means of distance travelled) mask
  // @returns mask or -1 if impossible
  // @param distance total distance travelled
  int FindBestMask(const game::CarState& state, double* distance);

  // Checks whether given throttle setup wont crash
  // @returns false if car crashes
  // @param distance total distance travelled
  bool CheckMask(int mask, const game::CarState& state, double* distance);

  // Log every schedule
  void Log(const game::CarState& state);

  game::CarTracker& car_tracker_;

  const game::Race& race_;

  std::vector<int> groups_;
  std::vector<double> schedule_;

  Strategy strategy_;
};

}  // namespace schedulers

#endif  // CPP_SCHEDULERS_BINARY_THROTTLE_SCHEDULER_H_
