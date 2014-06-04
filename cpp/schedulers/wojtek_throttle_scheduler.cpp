#include "schedulers/wojtek_throttle_scheduler.h"

#include <chrono>
#include <cstdio>
#include <cmath>

#include "gflags/gflags.h"
#include "utils/stopwatch.h"

DEFINE_bool(log_schedule, true, "");

namespace {
  template <typename T> int sgn(T val) {
    return (T(0) < val) - (val < T(0));
  }
}

namespace schedulers {

using game::CarState;

const vector<int> WojtekThrottleScheduler::GROUPS {1,1,2,2,2,2,4,4,4,8,4,4,4,2,1};

const int WojtekThrottleScheduler::HORIZON = std::accumulate(GROUPS.begin(),GROUPS.end(),0);
const vector<double> WojtekThrottleScheduler::values{0.0, 1.0}; // Values must be increasing

WojtekThrottleScheduler::WojtekThrottleScheduler(const game::Race& race,
    game::CarTracker& car_tracker, int time_limit)
  : race_(race), car_tracker_(car_tracker), best_schedule_(&car_tracker, HORIZON), branch_and_bound_(&car_tracker, HORIZON, GROUPS, values), log_file_("wojtek_data_log.csv", std::ofstream::out), tick_(0), time_limit_(time_limit)
{
   //Watchout: executing two WojtekThrottleSchedulers in pararell could be risky becase of log file (TODO)
   log_file_ << "tick," << "x," << "turbo," << "switch," << "a," << "v," << "dir," << "rad," << "piece_no," << "schedule_time," << "initial_schedule_safe," << "nodes_visited," << "leafs_visited," << "unsafe_cuts," << "ub_cuts," << "solution_improvements," << "schedule" << std::endl;
}

WojtekThrottleScheduler::~WojtekThrottleScheduler() {
    log_file_.close();
}

void WojtekThrottleScheduler::Schedule(const game::CarState& state, int game_tick) {
  tick_ += 1; //FIXME: get it as parameter

  utils::StopWatch stopwatch;
  best_schedule_.ShiftLeftFillSafe(state);
  best_schedule_.UpdateDistance(state); // Must do it, because we could have unpredicted turbo/switches, etc. ahead

  initial_schedule_safe_ = best_schedule_.IsSafe(state);
  if (!initial_schedule_safe_) {
    best_schedule_.Reset(state);
  }

  branch_and_bound_.Improve(state, best_schedule_);
  local_improver_.Improve(state, best_schedule_, 0.1);

  last_schedule_time_ = stopwatch.elapsed();
  Log(state);
}


void WojtekThrottleScheduler::PrintSchedule(const game::CarState& state, const Sched& schedule, int len) {
  for (int i=0; i<len; ++i)
    printf("%.2f ", schedule[i]);
  printf("\n");

  CarState next = state;
  for (int i=0; i<len; ++i) {
    next = car_tracker_.Predict(next, game::Command(schedule[i]));
    printf("%.1f ", next.position().angle());
  }
  printf("\n");
}

void WojtekThrottleScheduler::Log(const game::CarState& state) {
  if (FLAGS_log_schedule) {
    PrintSchedule(state, best_schedule_, std::min(HORIZON, 20));
  }

  game::Piece piece = race_.track().pieces()[state.position().piece()];
  log_file_ << tick_
            << ',' << throttle()
            << ',' << state.turbo_state().is_on() 
            << ',' << (int)state.switch_state() 
            << ',' << state.position().angle() 
            << ',' << state.velocity()
            << ',' << -sgn(piece.angle())
            << ',' << piece.radius() 
            << ',' << state.position().piece()
            << ',' << last_schedule_time_
            << ',' << initial_schedule_safe_
            << ',' << branch_and_bound_.stats().nodes_visited
            << ',' << branch_and_bound_.stats().leafs_visited
            << ',' << branch_and_bound_.stats().unsafe_cuts
            << ',' << branch_and_bound_.stats().ub_cuts
            << ',' << branch_and_bound_.stats().solution_improvements
            << ',';
  for (int i = 0; i < best_schedule_.size(); ++i)
    log_file_ << std::setprecision (2) << best_schedule_[i] << " ";
  log_file_ << std::endl;
  log_file_.flush();
}

}  // namespace schedulers
