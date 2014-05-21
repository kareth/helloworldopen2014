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
  static const vector<double> values; // possible throttle values to check
  static const double EGAP;

  BranchAndBound(game::CarTracker* car_tracker, int horizon);

  void Improve(const game::CarState& state, Sched& schedule);
  double LowerBound(const Sched& schedule);
  double UpperBound(const game::CarState& from_state, double from_dist, const Sched& schedule, int from);
  bool Branch(const game::CarState& state, Sched& schedule, double curr_dist, int from);

 private:
  double upper_bound_;
  double lower_bound_;
  int horizon_;
  game::CarTracker* car_tracker_;
  Sched best_;

  int nodes_visited_;
  int nodes_prunned_;
};

}

#endif
