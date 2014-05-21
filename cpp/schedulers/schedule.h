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
  void Shift(const game::CarState& state);
  void Reset(const game::CarState& state);
 
 private:
  game::CarTracker* car_tracker_;
};

}

#endif
