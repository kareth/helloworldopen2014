#include <cstdlib>
#include "schedulers/bb.h"

#include <numeric>

namespace schedulers {

using game::CarState;

const double BranchAndBound::EGAP = 0.0;

/* IDEAS_TODO:
 *  - store all nodes at the same time to decide which one to expand (the one with the highest lower bound). 
 *  - devise a better UpperBound than all 1.0. How?
 *  - if next tick will lead me to a piece with a different radius, apart from 0 and 1 consider a throttle that could finish before this new type of radius (if possible).
 *  - store all visited states (lane, position, v, alpha, omega) with their current best throttles and upper bounds. If can make a move to such already visited state (or better than) than we have a very good upper bound right away. Need to rethink this.
 *  - improve local improvement using some kind of gradients.
 */

BranchAndBound::BranchAndBound(game::CarTracker* car_tracker, int horizon, const vector<int>& groups, const vector<double>& values) 
  : horizon_(horizon), car_tracker_(car_tracker), best_(car_tracker, horizon), groups_(groups), values_(values), deadline_()
{ }

void BranchAndBound::Improve(const game::CarState& state, Sched& schedule, const utils::Deadline& deadline, 
      double distance_to_switch, double last_throttle) {
  //FIXME: switches
  // "global" for B&B
  best_ = schedule;
  lower_bound_ = LowerBound(best_);
  upper_bound_ = UpperBound(state, 0, best_, 0); 
  deadline_ = deadline;

  schedule.Reset(state);

  stats_ = Stats();

  Branch(state, schedule, 0, 0, 0);

  schedule = best_;
}

double BranchAndBound::LowerBound(const Sched& schedule) {
  return schedule.distance();
}

double BranchAndBound::UpperBound(const game::CarState& from_state, double from_dist, const Sched& schedule, int from) {
  // From the given state upper bound is when we try throttle 1.0 from now on to the end 
  // (without taking care of safety)

  return from_dist + car_tracker_->velocity_model().PredictDistance(from_state.velocity(), horizon_ - from, 1.0);
}

// Returning true if optimum found
bool BranchAndBound::Branch(const game::CarState& state, Sched& schedule, double curr_dist, int from, int from_group) {
  stats_.nodes_visited += 1;
  
  if (deadline_.HasExpired()) {
      // Pretend we have found optimum
      return true;
  }

  if (from >= horizon_) {
    stats_.leafs_visited += 1;
    schedule.UpdateDistance(curr_dist);
    //schedule.Print();

    bool better = (schedule.distance() > best_.distance());
    if (better) {
      if (car_tracker_->IsSafe(state)) {
        stats_.solution_improvements += 1;
        best_ = schedule;
        lower_bound_ = schedule.distance();
        if (upper_bound_ - lower_bound_ <= EGAP)
          return true;
      }
    }
    return false;
  }

  int group_size = groups_[from_group];
  for (int i = 0; i < values_.size(); ++i) {
    double throttle = values_[i];
    int group_size = groups_[from_group];

    CarState next = state;
    bool fail = false;
    for (int j=0; j<group_size; ++j) {
      next = car_tracker_->Predict(next, game::Command(throttle));

      // Cut fast
      if (!car_tracker_->crash_model().IsSafe(next.position().angle())) {
        fail = true;
        break; 
        // It is still possible to not crash when using a higher throttle,
        // so we should not return false here (it does not improve performance, but make the results worse)
      }
      schedule[from+j] = throttle;
    }
    if (fail) {
        stats_.unsafe_cuts += 1;
        continue;
    }

    double this_dist = car_tracker_->velocity_model().PredictDistance(state.velocity(), group_size, throttle);
    double ub = UpperBound(next, curr_dist + this_dist, schedule, from + group_size);

    // Prune
    if (ub - EGAP <= lower_bound_) {
      stats_.ub_cuts += 1;
      continue;
    }

    // This is an heuristic, but it slightly improves performance
    // It tries to cut leaves that probably will be still cut by IsSafe earlier
    if (horizon_ - from < 10)
        if (!car_tracker_->IsSafe(next, 4.0))
            return false;

    if (Branch(next, schedule, curr_dist + this_dist, from + groups_[from_group], from_group + 1)) {
      return true;
    }
  }
  return false;
}

}
