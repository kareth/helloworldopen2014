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

BranchAndBound::BranchAndBound(game::CarTracker* car_tracker, int horizon, 
    const vector<int>& groups, const vector<double>& values) 
  : horizon_(horizon), car_tracker_(car_tracker), best_(car_tracker, horizon), 
  groups_(groups), values_(values), deadline_()
{ }

void BranchAndBound::Improve(const game::CarState& state, Sched& schedule, 
    const utils::Deadline& deadline, double distance_to_switch, double last_throttle) {
  // "global" variables for B&B
  best_ = schedule;
  lower_bound_ = LowerBound(best_);
  upper_bound_ = UpperBound(state, 0, best_, 0); 
  deadline_ = deadline;
  distance_to_switch_ = distance_to_switch;
  last_throttle_ = last_throttle;

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

  return from_dist + car_tracker_->PredictDistance(from_state, horizon_ - from, 1.0);
}

// Returning true if optimum found
bool BranchAndBound::Branch(const game::CarState& state, Sched& schedule, double curr_dist, 
    int from, int from_group) {
  stats_.nodes_visited += 1;
  
  if (deadline_.HasExpired()) {
    return true; // Pretend we have found optimum
  }

  // Is this node a leaf?
  if (from >= horizon_) {
    stats_.leafs_visited += 1;
    schedule.UpdateDistance(curr_dist);

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
  for (double throttle : values_) {
    double saved_switch_position = schedule.switch_position();

    bool failx = false;
    // Check if we are near the switch and have to inject it
    if (distance_to_switch_ > 0 && schedule.switch_position() < 0) {
      for (int i = 0; i < group_size; ++i) {
        double d = curr_dist 
          + car_tracker_->PredictDistance(state, i+1, throttle);

        if (d >= distance_to_switch_) {
          // We have to inject the switch
          schedule.UpdateSwitchPosition(from + i);
          if (from + i == 0) {
            // The very first schedule position
            if (last_throttle_ > throttle)
              failx = true;
            throttle = last_throttle_;
          } else if (i == 0) {
            // The first position of the group
            if (schedule[from - 1] > throttle) {
              failx = true;
            }
            throttle = schedule[from - 1];
          } else {
            // Nothing to do (all throttles in the group have the same value)
          }
          break;
        }
      }
    }
    if (failx) break; //FIXME

    CarState next = state;
    bool fail = false;
    for (int j = 0; j < group_size; ++j) {
      next = car_tracker_->Predict(next, game::Command(throttle));
      // Cut fast if not safe
      if (!car_tracker_->crash_model().IsSafe(next.position().angle())) {
        fail = true;
        break; 
      }
      schedule[from + j] = throttle;
    }
    if (fail) {
      stats_.unsafe_cuts += 1;
      schedule.UpdateSwitchPosition(saved_switch_position);
      continue;
    }

    double this_dist = car_tracker_->PredictDistance(state, group_size, throttle);
    double ub = UpperBound(next, curr_dist + this_dist, schedule, from + group_size);

    // Prune
    if (ub - EGAP <= lower_bound_) {
      stats_.ub_cuts += 1;
      schedule.UpdateSwitchPosition(saved_switch_position);
      continue;
    }

    // This is an heuristic, but it slightly improves performance
    // It tries to cut leaves that probably will be still cut by IsSafe earlier
    if (horizon_ - from < 10 && !car_tracker_->IsSafe(next, 4.0)) {
      schedule.UpdateSwitchPosition(saved_switch_position);
      return false;
    }

    if (Branch(next, schedule, curr_dist + this_dist, from + groups_[from_group], from_group + 1)) {
      return true;
    }
    schedule.UpdateSwitchPosition(saved_switch_position);
    //TODO: Sort out exits from this function. Auxiliary function for each values is needed.
    //To many points of exit. To many places, where
    //I have to add the cleaning code: schedule.UpdateSwitchPosition(saved_switch_position);
  }
  return false;
}

}
