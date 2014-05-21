#include <cstdlib>
#include "schedulers/bb.h"

#include <numeric>

namespace schedulers {

using game::CarState;

const vector<double> BranchAndBound::values{0.0, 1.0};
const double BranchAndBound::EGAP = 0.0;

BranchAndBound::BranchAndBound(game::CarTracker* car_tracker, int horizon, const vector<int>& groups) 
  : horizon_(horizon), car_tracker_(car_tracker), best_(car_tracker, horizon), groups_(groups)
{ }

void BranchAndBound::Improve(const game::CarState& state, Sched& schedule) {
  best_ = schedule;
  //TODO: improve schedule by LS first? Last throttle 1?
  lower_bound_ = LowerBound(best_);
  upper_bound_ = UpperBound(state, 0, best_, 0); 

  //printf("Init: (d=%.1f): ", best_.distance); best_.Print();
  schedule.Reset(state);

  nodes_visited_ = 0;
  nodes_prunned_ = 0;
  Branch(state, schedule, 0, 0, 0);

  //printf("vis=%d, prun=%d", nodes_visited_, nodes_prunned_);

  schedule = best_;
}

double BranchAndBound::LowerBound(const Sched& schedule) {
  return schedule.distance;
}

double BranchAndBound::UpperBound(const game::CarState& from_state, double from_dist, const Sched& schedule, int from) {
  // Try all 1.0 without taking care of safety
  CarState next = from_state;
  for (int i = from; i < horizon_; ++i) {
    next = car_tracker_->Predict(next, game::Command(1.0));
  }
  return from_dist + car_tracker_->DistanceBetween(from_state.position(), 
        next.position());
}

bool BranchAndBound::Branch(const game::CarState& state, Sched& schedule, double curr_dist, int from, int from_group) {

  nodes_visited_ += 1;

  if (from >= horizon_) {
    bool better = (curr_dist > best_.distance);
    //printf ("found dist = %.1f\n", curr_dist);

    if (better) {
      if (car_tracker_->IsSafe(state)) {
        //printf("Found better (d=%.1f):", curr_dist); schedule.Print();
        best_ = schedule;
        best_.distance = curr_dist;
        lower_bound_ = best_.distance;
        if (upper_bound_ - lower_bound_ <= EGAP)
          return true;
      }
    }
    return false;
  }

  for (int i = 0; i < values.size(); ++i) {
    double throttle = values[i];
    CarState next = state;
    bool fail = false;
    for (int i=0; i<groups_[from_group]; ++i) {
      next = car_tracker_->Predict(next, game::Command(throttle));

      // Cut fast
      if (!car_tracker_->crash_model().IsSafe(next.position().angle())) {
        nodes_prunned_ += 1;
        fail = true;
        break;
      }
      schedule.throttles[from+i] = throttle;
    }
    if (fail) continue;
    double this_dist = car_tracker_->DistanceBetween(state.position(), next.position());

    double ub = UpperBound(next, curr_dist + this_dist, schedule, from + groups_[from_group]);

    // Prune
    if (ub - EGAP <= lower_bound_) {
      nodes_prunned_ += 1;
      continue;
    }

    if (Branch(next, schedule, curr_dist + this_dist, from + groups_[from_group], from_group + 1)) {
      return true;
    }
  }
  return false;
}

}
