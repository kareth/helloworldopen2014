#include "schedulers/schedule.h"

namespace schedulers {

//TODO: Why the compiler asks me for explicit schedulers::?
schedulers::Sched::Sched(game::CarTracker* car_tracker, int horizon)
  : car_tracker_(car_tracker), throttles_(horizon, 0), distance_(0) {
  // There is no state given, so we assume no initial speed, thus distance = 0, 
  // but this state of Sched is actually invalid, because no CarState is provided
}

game::CarState schedulers::Sched::Predict(const game::CarState& state) {
  game::CarState next = state;
  for (int i = 0; i<size(); ++i) {
    next = car_tracker_->Predict(next, game::Command(throttles_[i]));
  }
  return next;
}

void schedulers::Sched::UpdateDistance(const game::CarState& state) {
  distance_ = car_tracker_->velocity_model().PredictDistance(state.velocity(), throttles_);
}

// Whether schedule is safe
bool schedulers::Sched::IsSafe(const game::CarState& state) {
  //TODO: Improve performance by adding "from"
  
  game::CarState next = state;
  // Are all scheduled states safe?
  for (int i = 0; i<size(); ++i) {
    next = car_tracker_->Predict(next, game::Command(throttles_[i]));
    if (!car_tracker_->crash_model().IsSafe(next.position().angle()))
      return false;
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

void schedulers::Sched::ShiftRight(const game::CarState& state, double throttle0) {
  for (int i =size()-1; i>=1; --i) {
    throttles_[i] = throttles_[i-1];
  }
  throttles_[0] = throttle0;
  UpdateDistance(state);
}

void schedulers::Sched::ShiftLeftFillSafe(const game::CarState& state) {
  ShiftLeft(state);
  throttles_[size() - 1] = 1.0;
  if (IsSafe(state)) {
    UpdateDistance(state);
  } else {
    throttles_[size() - 1] = 0.0;
  }
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
