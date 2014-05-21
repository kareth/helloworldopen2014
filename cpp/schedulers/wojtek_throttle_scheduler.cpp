#include "schedulers/wojtek_throttle_scheduler.h"

#include <chrono>

#include "gflags/gflags.h"

namespace schedulers {

using game::CarState;

const int WojtekThrottleScheduler::HORIZON = 15;
const vector<double> WojtekThrottleScheduler::values{0.0, 1.0};

WojtekThrottleScheduler::WojtekThrottleScheduler(const game::Race* race,
    game::CarTracker* car_tracker)
  : race_(race), car_tracker_(car_tracker), best_schedule_(car_tracker, HORIZON), bb_(car_tracker, HORIZON) {
}

void WojtekThrottleScheduler::Schedule(const game::CarState& state) {
  best_schedule_.Shift(state);
  if (!best_schedule_.IsSafe(state))
    best_schedule_.Reset(state);

  best_schedule_.throttles[HORIZON-1]=1.0;
  if (best_schedule_.IsSafe(state)) {
    best_schedule_.distance = best_schedule_.Distance(state);
  } else {
    best_schedule_.throttles[HORIZON-1]=0.0;
  }

  bb_.Improve(state, best_schedule_);

  VNS(state, best_schedule_);

  /*if (!Optimize(state)) {
    VNS(state, best_schedule_);
  }*/
  throttle_ = best_schedule_.throttles[0];
  printf("\n");
  for (int i=0; i<HORIZON; ++i)
    printf("%.1f ", best_schedule_.throttles[i]);
  printf("\n");
}


bool WojtekThrottleScheduler::Optimize(const game::CarState& state) {
  Sched schedule(best_schedule_);
  best_schedule_.distance = -1;

  //TODO: Initialize best_schedule to 0

  // Makes the simulation quicker
  for (int i = 0; i < schedule.size(); ++i) {
    schedule.throttles[i] = 1.0;
  }
  schedule.distance = schedule.Distance(state);
  if (schedule.IsSafe(state)) {
    best_schedule_ = schedule;
    return true;
  }

  schedule.distance = 0;
  Check(state, 0, schedule);

  return false;
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
    tmp.distance = tmp.Distance(state);
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

bool WojtekThrottleScheduler::Improve(const game::CarState& state, Sched& schedule, int step) {
  bool improved = false;
  for (int i=0; i<schedule.size(); ++i) {
    improved = ImproveOne(state, schedule, i, step);
  }
  return improved;
}

bool WojtekThrottleScheduler::Check(const game::CarState& state, int from, Sched& schedule) {
  if (from >= HORIZON) {
    bool better = (schedule.distance > best_schedule_.distance);

    if (better) {
      if (car_tracker_->IsSafe(state)) {
        best_schedule_ = schedule;
        bool optimal = true;
        for (int i=0;i<schedule.throttles.size(); ++i) {
          if (schedule.throttles[i] != 1.0) {
            optimal = false;
          }
        }
        if (optimal) return true;
      }
    }
    return false;
  }

  for (int i = 0; i < values.size(); ++i) {
    double throttle = values[i];
    CarState next = car_tracker_->Predict(state, game::Command(throttle));

    // Cut fast
    if (!car_tracker_->crash_model().IsSafe(next.position().angle())) {
        continue;
    }

    double this_dist = car_tracker_->DistanceBetween(state.position(), 
        next.position());
    //TODO: Jesli nie odcinam po dystansie, moglbym tego tutaj nie liczyc tylko na koncu

    schedule.throttles[from] = throttle;
    schedule.distance += this_dist;
    Check(next, from + 1, schedule);
    schedule.distance -= this_dist;
  }
  return false;
}

}  // namespace schedulers
