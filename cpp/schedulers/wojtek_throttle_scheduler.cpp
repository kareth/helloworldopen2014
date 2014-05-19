#include "schedulers/wojtek_throttle_scheduler.h"

#include <chrono>

#include "gflags/gflags.h"

namespace schedulers {

using game::CarState;

WojtekThrottleScheduler::WojtekThrottleScheduler(const game::Race* race,
    const game::CarTracker* car_tracker)
  : race_(race), car_tracker_(car_tracker) {
}

void WojtekThrottleScheduler::Schedule(const game::CarState& state) {
  throttle_ = 0.6;
}

}  // namespace schedulers
