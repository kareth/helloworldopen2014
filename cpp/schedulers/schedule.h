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
  vector<double> throttles;
  double distance = 0;
  size_t size() const { return throttles.size(); }

  game::CarState Predict(const game::CarState& state);
  double Distance(const game::CarState& state);
  bool IsSafe(const game::CarState& state);
  void ShiftLeft(const game::CarState& state);
  void ShiftLeftFillSafe(const game::CarState& state);
  void ShiftRight(const game::CarState& state, double throttle0);
  void Reset(const game::CarState& state);
  void Print();
  void Widen(const game::CarState& state, int num_values);
  void Shorten(const game::CarState& state, int num_values);
 
 private:
  game::CarTracker* car_tracker_;
};

}

#endif
