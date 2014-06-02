#include "schedulers/wojtek_throttle_scheduler.h"

#include <chrono>
#include <cstdio>
#include <cmath>

#include "gflags/gflags.h"
#include "utils/stopwatch.h"

namespace {
  template <typename T> int sgn(T val) {
    return (T(0) < val) - (val < T(0));
  }
}

namespace schedulers {

using game::CarState;

vector<int> groups {1,1,2,2,4,4,4,4,4,4,4,2,1};
//vector<int> groups {1,1,1,1,1,1,1,1,1,1,1,1,1};
const int WojtekThrottleScheduler::HORIZON = std::accumulate(groups.begin(),groups.end(),0);
const vector<double> WojtekThrottleScheduler::values{0.0, 1.0};

WojtekThrottleScheduler::WojtekThrottleScheduler(const game::Race& race,
    game::CarTracker& car_tracker) //TODO: Reference
  : race_(race), car_tracker_(car_tracker), best_schedule_(&car_tracker, HORIZON), bb_(&car_tracker, HORIZON, groups, values), log_file_("wojtek_data_log.csv", std::ofstream::out), tick_(0)
{
    //Watchout: executing two WojtekThrottleSchedulers in pararell could be risky becase of log file (TODO)
   log_file_ << "tick," << "lap," << "x," << "turbo," << "switch," << "a," << "v," << "dir," << "rad," << "piece_no," << "schedule_time," << "schedule" << std::endl;
}

WojtekThrottleScheduler::~WojtekThrottleScheduler() {
    log_file_.close();
}

void WojtekThrottleScheduler::Schedule(const game::CarState& state) {
  tick_ += 1; //FIXME: get it as parameter

  printf("tick: %d\n", tick_);
  printf("prediction with all zeros:\n");
  PrintSchedule(state, Sched(&car_tracker_, HORIZON), HORIZON);

  utils::StopWatch stopwatch;
  best_schedule_.ShiftLeftFillSafe(state);
  best_schedule_.UpdateDistance(state); // Must do it, because we could have unpredicted turbo/switches, etc. ahead
  if (!best_schedule_.IsSafe(state))
    best_schedule_.Reset(state);

  bb_.Improve(state, best_schedule_);

  Improve(state, best_schedule_, 0.1);

  throttle_ = best_schedule_.throttles[0];

  last_schedule_time_ = stopwatch.elapsed();
  Log(state);
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

void WojtekThrottleScheduler::PrintSchedule(const game::CarState& state, const Sched& schedule, int len) {
  for (int i=0; i<len; ++i)
    printf("%.2f ", schedule.throttles[i]);
  printf("\n");

  CarState next = state;
  for (int i=0; i<len; ++i) {
    next = car_tracker_.Predict(next, game::Command(schedule.throttles[i]));
    printf("%.1f ", next.position().angle());
  }
  printf("\n");
}

void WojtekThrottleScheduler::Log(const game::CarState& state) {
  //PrintSchedule(state, best_schedule_, std::min(HORIZON, 20));

  //TODO: Tick and lap number
  game::Piece piece = race_.track().pieces()[state.position().piece()];
  log_file_ << ',' << tick_
            << ',' << 0
            << ',' << throttle()
            << ',' << state.turbo_state().is_on() 
            << ',' << (int)state.switch_state() 
            << ',' << state.position().angle() 
            << ',' << state.velocity()
            << ',' << -sgn(piece.angle())
            << ',' << piece.radius() 
            << ',' << state.position().piece()
            << ',' << last_schedule_time_
            << ',';
  for (int i = 0; i < best_schedule_.size(); ++i)
    log_file_ << std::setprecision (2) << best_schedule_.throttles[i] << " ";
  log_file_ << std::endl;
  log_file_.flush();
}

}  // namespace schedulers
