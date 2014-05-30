#include "schedulers/magic_throttle_scheduler.h"

#include <chrono>

#include "gflags/gflags.h"

namespace schedulers {

using game::CarState;

const int MagicThrottleScheduler::HORIZON = 50;
const int MagicThrottleScheduler::N = 100;

MagicThrottleScheduler::MagicThrottleScheduler(const game::Race& race,
    game::CarTracker& car_tracker, int time_limit)
  : race_(race), car_tracker_(car_tracker), best_schedule_(&car_tracker, HORIZON), time_limit_(time_limit) 
{
}

void MagicThrottleScheduler::Schedule(const game::CarState& state) {
  best_schedule_.ShiftLeftFillSafe(state);
  best_schedule_.UpdateDistance(state);      // Must do it, because throttle could have changed!
  //if (!best_schedule_.IsSafe(state))       // TODO: I'm not sure. Maybe we should make it safe?
  //  best_schedule_.Reset(state);
  
  ImproveByMagic(state, best_schedule_);

  //TODO: Check if safe, make some local changes to make it safe

  Log(state);
}

void MagicThrottleScheduler::ImproveByMagic(const game::CarState& state, Sched& schedule) {
    //TODO
}

void MagicThrottleScheduler::Log(const game::CarState& state) {
  for (int i=0; i<HORIZON; ++i)
    printf("%.1f ", best_schedule_.throttles[i]);
  printf("\n");

  std::cout << "(" << state.position().piece() << ")" << " angle: " <<
    state.position().angle() << " velocity: " << state.velocity() <<
    std::endl;
}

}  // namespace schedulers
