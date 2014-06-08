#ifndef CPP_SCHEDULERS_SCHEDULE_H_
#define CPP_SCHEDULERS_SCHEDULE_H_

#include <vector>
#include <array>
#include <cmath>
#include "game/car_tracker.h"

namespace schedulers {

class Sched {
 public:
  Sched(game::CarTracker* car_tracker_, int horizon);

  int size() const { return throttles_.size(); }
  const vector<double>& throttles() const { return throttles_; }
  double distance() const { return distance_; }
  double switch_position() const { return switch_position_; }

  void UpdateDistance(const game::CarState& state);
  void UpdateDistance(double new_distance); // Explicitely

  // Number of ticks until switch. <0 if no switch ahead
  int GetTicksToTheRequiredSwitch(const game::CarState& state, double distance_to_switch);

  // Afterwards, you should call UpdateDistance in most cases
  void UpdateSwitchPosition(int switch_position);

  // Sets throttles[switch_position] = throttles[switch_position-1]. 
  // You may need to call UpdateDistance afterwards.
  void CorrectSwitch(const game::CarState& state, double last_throttle);

  // Return if succeded (the resulting schedule is safe and switch-correct).
  // 0 <= new_switch_position < size()
  // Distance is not updated
  bool TryUpdateSwitchPosition(const game::CarState& state, int new_switch_position,
    double distance_to_switch, double last_throttle);

  // Does not change the safety or correctness
  void RemoveSwitch();

  // distance_to_switch < 0 if no switch ahead.
  bool IsSafe(const game::CarState& state, double distance_to_switch, double last_throttle);
  void ShiftLeft(const game::CarState& state);
  void ShiftLeftFillSafe(const game::CarState& state, double distance_to_switch, double last_throttle);

  void Reset(const game::CarState& state);
  void Print();

  double& operator[](std::size_t idx) { return throttles_[idx]; };
  const double operator[](std::size_t idx) const { return throttles_[idx]; };


 private:
  vector<double> throttles_; // Breaks encapsulation, but need it, unfortunatelly
  game::CarTracker* car_tracker_;
  double distance_;
  int switch_position_; // no switch if < 0
};

}

#endif
