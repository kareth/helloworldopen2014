#include "schedulers/schedule.h"

namespace schedulers {

//TODO: Why the compiler asks me for explicit schedulers::?
schedulers::Sched::Sched(game::CarTracker* car_tracker, int horizon)
  : car_tracker_(car_tracker), throttles_(horizon, 0), distance_(0), switch_position_(-1) {
  // There is no state given, so we assume no initial speed, thus distance = 0, 
  // but this state of Sched is actually invalid, because no CarState is provided
}

void schedulers::Sched::UpdateDistance(const game::CarState& state) {
  distance_ = car_tracker_->velocity_model().PredictDistance(state.velocity(), throttles_);
}

void schedulers::Sched::UpdateDistance(double new_distance) {
  distance_ = new_distance;
}

void schedulers::Sched::CorrectSwitch(const game::CarState& state, double last_throttle) {
  bool switch_is_planned = (switch_position_ >= 0);
  if (switch_is_planned) {
    if (switch_position_ > 0) {
      last_throttle = throttles_[switch_position_ - 1];
    }
    throttles_[switch_position_] = last_throttle;
  }
}

//TODO: Improve performance by adding "from"
// Whether schedule is safe
bool schedulers::Sched::IsSafe(const game::CarState& state, double distance_to_switch, double last_throttle) {
  bool check_switch = (distance_to_switch >= 0);
  
  if (check_switch) {
    if (switch_position_ > 0) {
      last_throttle = throttles_[switch_position_ - 1];
    }
    if (throttles_[switch_position_] != throttles_[switch_position_-1]) {
      // It still could be safe, but I treat it as incorrect. So it should be first corrected
      // by calling CorrectSwitch().
      std::cerr << "Schedule has incorrect throttles" << std::endl;
      return false;
    }
  }
  
  int distance = 0;
  game::CarState next = state;
  // Are all scheduled states safe?
  for (int pos = 0; pos < size(); ++pos) {
    next = car_tracker_->Predict(next, game::Command(throttles_[pos]));
    if (!car_tracker_->crash_model().IsSafe(next.position().angle()))
      return false;

    if (check_switch) {
      // Check whether we done the switch
      distance += next.velocity();
      if (distance >= distance_to_switch) {
        bool switch_already_done = (switch_position_ >= 0 && switch_position_ <= pos);
        if (!switch_already_done) {
          // Switch was supposed to be done before distance_to_switch, but it was not
          return false;
        }
      }
    }
  }
  // Is the last state safe?
  return car_tracker_->IsSafe(next);
}

// No guarantee it is still safe
void schedulers::Sched::ShiftLeft(const game::CarState& state) {
  for (int i =0; i<size()-1; ++i) {
    throttles_[i] = throttles_[i+1];
  }
  throttles_[size()-1] = 0.0;
  UpdateDistance(state);
}

void schedulers::Sched::ShiftLeftFillSafe(const game::CarState& state, double distance_to_switch, double last_throttle) {
  ShiftLeft(state);
  // Try to set 1.0
  throttles_[size() - 1] = 1.0;
  if (IsSafe(state, distance_to_switch, last_throttle)) {
    return ;
  }

  // If not then try to set the last value (it might work if a switch appeared in the horizon)
  if (distance_to_switch >= 0) {
    throttles_[size() - 1] = throttles_[size() - 2];
    if (IsSafe(state, distance_to_switch, last_throttle)) {
      return;
    }
  }

  // Fall back to 0.0
  throttles_[size() - 1] = 0.0;
}

void schedulers::Sched::Reset(const game::CarState& state) {
  for (int i = 0; i<size(); ++i) {
    throttles_[i] = 0;
  }
  UpdateDistance(state);
}

void schedulers::Sched::Print() {
  for (int i = 0; i < size(); ++i) {
    printf("%.1f ", throttles_[i]);
  }
  printf(" [%.1f]\n", distance_);
}

} // namespace
