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

int schedulers::Sched::GetTicksToTheRequiredSwitch(const game::CarState& state, double distance_to_switch) {
  if (distance_to_switch < 0)
    return -1;

  int distance = 0;
  game::CarState next = state;
  for (int pos = 0; pos < size(); ++pos) {
    //TODO(minor) I could use the quicker PredictVelocity (waiting for turbo)
    next = car_tracker_->Predict(next, game::Command(throttles_[pos]));

    distance += next.velocity();
    if (distance >= distance_to_switch) {
      if (throttles_[pos] == 0.0) {
        return pos;
      } else {
        // +1 might be too much in some cases, but I am looking for the upper bound
        return pos + 1; 
      }
    }
  }
  // Distance longer then the current horizon. We will not care yet.
  return -1;
}

void schedulers::Sched::UpdateSwitchPosition(int switch_position) { 
  switch_position_ = switch_position; 
}

void schedulers::Sched::RemoveSwitch() { 
  UpdateSwitchPosition(-1);
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

// Return if succeded (the resulting schedule is safe and switch-correct).
bool schedulers::Sched::TryUpdateSwitchPosition(const game::CarState& state, int new_switch_position, 
    double distance_to_switch, double last_throttle) {
  int saved_switch_position = switch_position_;
  double saved_throttle = throttles_[new_switch_position];

  UpdateSwitchPosition(new_switch_position);
  CorrectSwitch(state, last_throttle);

  bool success = IsSafe(state, distance_to_switch, last_throttle);

  if (!success) {
    // Undo if failed
    switch_position_ = saved_switch_position;
    throttles_[new_switch_position] = saved_throttle;
  }

  return success;
}

//TODO: Improve performance by adding "from"
// Whether schedule is safe and switch-correct
bool schedulers::Sched::IsSafe(const game::CarState& state, double distance_to_switch, double last_throttle) {
  bool check_switch = (distance_to_switch >= 0);
  
  int distance = 0;
  game::CarState next = state;
  // Are all scheduled states safe and switch-correct?
  for (int pos = 0; pos < size(); ++pos) {
    // Are angles safe?
    next = car_tracker_->Predict(next, game::Command(throttles_[pos]));
    if (!car_tracker_->crash_model().IsSafe(next.position().angle())) {
      //std::cerr << "here: " << pos << std::endl;
      return false;
    }

    if (check_switch) {
      // Check whether switch is planned in tick from range [0,pos]
      distance += next.velocity();
      if (distance_to_switch <= distance) {
        bool switch_done = (switch_position_ >= 0 && switch_position_ <= pos);
        if (!switch_done) {
          // Switch was supposed to be done before distance_to_switch, but it was not
         // std::cerr << "check: " << pos << " " << distance << " " << distance_to_switch << std::endl;
          return false;
        }
      }
    }
  }

  // Check whether the planned switch is correct
  if (check_switch && distance_to_switch <= distance) {
    if (switch_position_ < 0) {
        // We should have switch before the end of the horizon, but we have not
        std::cerr << "bbbb: " << distance << " " << distance_to_switch << std::endl;
        return false; 
    }

    // Now, check if there are two consecutive throttles
    if (switch_position_ > 0) {
      last_throttle = throttles_[switch_position_ - 1];
    }
    if (throttles_[switch_position_] != last_throttle) {
      // It still could be safe, but I treat it as incorrect. So it should be first corrected
      // by calling CorrectSwitch().
      std::cerr << "Schedule has incorrect throttles" << std::endl;
      return false;
    }
  }
  
  // This check is last, for efficiency
  return car_tracker_->IsSafe(next);
}

// No guarantee it is still safe
void schedulers::Sched::ShiftLeft(const game::CarState& state) {
  for (int i = 0; i < size() - 1; ++i) {
    throttles_[i] = throttles_[i + 1];
  }
  throttles_[size() - 1] = 0.0;
  switch_position_ -= 1;

  UpdateDistance(state);
}

void schedulers::Sched::ShiftLeftFillSafe(const game::CarState& state, double distance_to_switch, double last_throttle) {
  ShiftLeft(state);
  // Try 1.0 at the end
  throttles_[size() - 1] = 1.0;
  if (IsSafe(state, distance_to_switch, last_throttle)) {
    return ;
  }

  // If not then try to set the last value + switch at the end.
  // Tt might work if a switch appeared in the horizon
  if (distance_to_switch >= 0) {
    throttles_[size() - 1] = throttles_[size() - 2];

    int saved_switch_position_ = switch_position_;
    switch_position_ = size() - 1;
    if (IsSafe(state, distance_to_switch, last_throttle)) {
      return;
    }
    // Undo switch position change
    switch_position_ = saved_switch_position_;
  }

  // Otherwise, fall back to 0.0
  throttles_[size() - 1] = 0.0;
}

void schedulers::Sched::Reset(const game::CarState& state) {
  for (int i = 0; i<size(); ++i) {
    throttles_[i] = 0;
  }
  switch_position_ = -1;
  UpdateDistance(state);
}

void schedulers::Sched::Print() {
  for (int i = 0; i < size(); ++i) {
    printf("%.1f ", throttles_[i]);
  }
  printf(" [%.1f] s=%d\n", distance_, switch_position_);
}

} // namespace
