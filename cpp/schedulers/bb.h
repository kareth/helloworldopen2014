#ifndef CPP_SCHEDULERS_BB_H_
#define CPP_SCHEDULERS_BB_H_

#include <vector>
#include <array>
#include <cmath>
#include "game/car_tracker.h"
#include "schedulers/schedule.h"

namespace schedulers {

class BranchAndBound {
 public:
  BranchAndBound(game::CarTracker* car_tracker, int horizon);

  void Improve(Sched& schedule);
 
 private:
  double upper_bound_;
  double lower_bound_;
  int horizon_;
  game::CarTracker* car_tracker_;
};

}

#endif
