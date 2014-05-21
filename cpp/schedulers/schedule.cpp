#include "schedulers/schedule.h"

namespace schedulers {

//TODO: Why the compiler asks me for explicit schedulers::?
schedulers::Sched::Sched(game::CarTracker* car_tracker, int horizon)
  : throttles(horizon), car_tracker_(car_tracker) {
  for (int i = 0; i<horizon; ++i)
    throttles[i] = 0.0;
  distance = 0; // There is no state given, so we assume no initial speed
}

game::CarState schedulers::Sched::Predict(const game::CarState& state) {
  game::CarState next = state;
  for (int i = 0; i<size(); ++i) {
    next = car_tracker_->Predict(next, game::Command(throttles[i]));
  }
  return next;
}

void schedulers::Sched::UpdateDistance(const game::CarState& state) {
  game::CarState last = Predict(state);
  distance = car_tracker_->DistanceBetween(state.position(), last.position());
}

// Whether schedule is safe
bool schedulers::Sched::IsSafe(const game::CarState& state) {
  //TODO: Improve performance by adding "from"
  
  game::CarState next = state;
  // Are all scheduled states safe?
  //printf("check: ");
  for (int i = 0; i<size(); ++i) {
    next = car_tracker_->Predict(next, game::Command(throttles[i]));
   // printf("%.1f ", next.position().angle());
    if (!car_tracker_->crash_model().IsSafe(next.position().angle()))
      return false;
  }
  //printf("\n");
  // Is the last state safe?
  return car_tracker_->IsSafe(next);
}

void schedulers::Sched::ShiftLeft(const game::CarState& state) {
  for (int i =0; i<size()-1; ++i) {
    throttles[i] = throttles[i+1];
  }
  throttles[size()-1] = 0.0;
  UpdateDistance(state);
  //TODO: Should be safe, but I have to check it to be sure!
}

void schedulers::Sched::ShiftRight(const game::CarState& state, double throttle0) {
  for (int i =size()-1; i>=1; --i) {
    throttles[i] = throttles[i-1];
  }
  throttles[0] = throttle0;
  UpdateDistance(state);
}

void schedulers::Sched::ShiftLeftFillSafe(const game::CarState& state) {
  ShiftLeft(state);
  throttles[size() - 1] = 1.0;
  if (IsSafe(state)) {
    UpdateDistance(state);
  } else {
    throttles[size() - 1] = 0.0;
  }
}

void schedulers::Sched::Reset(const game::CarState& state) {
  for (int i = 0; i<size()-1; ++i) {
    throttles[i] = 0;
  }
  UpdateDistance(state);
}

void schedulers::Sched::Print() {
  for (int i = 0; i < size(); ++i) {
    printf("%.1f ", throttles[i]);
  }
  printf("\n");
}

void schedulers::Sched::Widen(const game::CarState& state, int num_values) {
  for (int i =0;i<num_values; ++i)
    throttles.push_back(0.0);
  UpdateDistance(state);
}

void schedulers::Sched::Shorten(const game::CarState& state, int num_values) {
  for (int i =0;i<num_values; ++i)
    throttles.pop_back();
  UpdateDistance(state);
}

} // namespace
