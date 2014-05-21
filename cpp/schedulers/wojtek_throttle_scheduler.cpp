#include "schedulers/wojtek_throttle_scheduler.h"

#include <chrono>

#include "gflags/gflags.h"

namespace schedulers {

using game::CarState;

vector<int> groups {1,1,2,2,4,4,4,4,4,4,4};
const int WojtekThrottleScheduler::HORIZON = std::accumulate(groups.begin(),groups.end(),0);
const vector<double> WojtekThrottleScheduler::values{0.0, 1.0};

WojtekThrottleScheduler::WojtekThrottleScheduler(const game::Race* race,
    game::CarTracker* car_tracker)
  : race_(race), car_tracker_(car_tracker), best_schedule_(car_tracker, HORIZON), bb_(car_tracker, HORIZON, groups) {
}

void WojtekThrottleScheduler::Schedule(const game::CarState& state) {
  best_schedule_.ShiftLeftFillSafe(state);
  if (!best_schedule_.IsSafe(state))
    best_schedule_.Reset(state);

  bb_.Improve(state, best_schedule_);

  Improve(state, best_schedule_, 0.1);

  //VNS(state, best_schedule_);

  /*if (!Optimize(state)) {
    VNS(state, best_schedule_);
  }*/

  throttle_ = best_schedule_.throttles[0];
  printf("\n");
  for (int i=0; i<HORIZON; ++i)
    printf("%.2f ", best_schedule_.throttles[i]);
  printf("\n");
}

bool WojtekThrottleScheduler::VNS(const game::CarState& state, Sched& schedule) {

  const int STEPS = 100;
  const double STEP = 0.1;

  bool improved = false;
  for (int i = 0; i < STEPS; ++i) {
    int idx = rand() % 3;
    if (schedule.throttles[idx] == 0.0)
      continue;

    Sched tmp(schedule);

    tmp.throttles[idx] = 0.0;

    for (int j = 0; j < tmp.size(); ++j) if (j != idx)
      ImproveOne(state, tmp, j, STEP);
    ImproveOne(state, tmp, idx, STEP);
    tmp.UpdateDistance(state);
    if (tmp.distance > schedule.distance) {
      schedule = tmp;
      improved = true;
      printf("IMPROVED");
    }
  }
  return improved;
}

bool WojtekThrottleScheduler::ImproveOne(const game::CarState& state, Sched& schedule, int idx, double step) {
  bool improved = false;
  while (schedule.throttles[idx] + step <= 1.0) {
    schedule.throttles[idx] += step;
    if (!schedule.IsSafe(state)) {
      schedule.throttles[idx] -= step;
      break;
    }
    // If throttle was increased there has to be distance improvement
    improved = true;
  }
  return improved;
}

bool WojtekThrottleScheduler::Improve(const game::CarState& state, Sched& schedule, double step) {
  bool improved = false;
  for (int i=0; i<schedule.size(); ++i) {
    improved = ImproveOne(state, schedule, i, step);
  }
  if (improved)
    schedule.UpdateDistance(state);
  return improved;
}

}  // namespace schedulers
