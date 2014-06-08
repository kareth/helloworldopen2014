#ifndef CPP_SCHEDULERS_BB_H_
#define CPP_SCHEDULERS_BB_H_

#include <vector>
#include <array>
#include <cmath>

#include "game/car_tracker.h"
#include "schedulers/schedule.h"
#include "utils/deadline.h"

namespace schedulers {

class BranchAndBound {
 public:
  class Stats {
  public:
   int nodes_visited = 0;
   int leafs_visited = 0;
   int unsafe_cuts = 0;
   int ub_cuts = 0;
   int solution_improvements = 0;
  };

  static const double EGAP;

  BranchAndBound(game::CarTracker* car_tracker, int horizon, const vector<int>& groups, const vector<double>& values);

  void Improve(const game::CarState& state, Sched& schedule, const utils::Deadline& deadline, double distance_to_switch, double last_throttle);

  Stats stats() { return stats_; }

 private:
  double LowerBound(const Sched& schedule);
  double UpperBound(const game::CarState& from_state, double from_dist, const Sched& schedule, int from);
  bool Branch(const game::CarState& state, Sched& schedule, double curr_dist, int from, int from_group);

  double upper_bound_;
  double lower_bound_;
  double distance_to_switch_;
  double last_throttle_;
  int horizon_;
  game::CarTracker* car_tracker_;
  Sched best_;
  Stats stats_;
  utils::Deadline deadline_;

  vector<int> groups_;
  vector<double> values_; // possible throttle values to check
};

}

#endif
