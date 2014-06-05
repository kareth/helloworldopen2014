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

const vector<int> WojtekThrottleScheduler::DEFAULT_GROUPS {1,1,4,2,2,4,1,8,8,4,4,2};
const vector<int> WojtekThrottleScheduler::QUICK_GROUPS {1,2,4,4,2};

const vector<double> WojtekThrottleScheduler::values{0.0, 1.0}; // Values must be increasing

WojtekThrottleScheduler::WojtekThrottleScheduler(const game::Race& race,
    game::CarTracker& car_tracker, int time_limit, const vector<int>& groups)
  : race_(race), car_tracker_(car_tracker), 
    groups_(groups),
    horizon_(std::accumulate(groups.begin(), groups.end(), 0)),
    best_schedule_(&car_tracker, horizon_),
    branch_and_bound_(&car_tracker, horizon_, groups_, values), log_file_("wojtek_data_log.csv", std::ofstream::out), time_limit_(time_limit)
{
   //Watchout: executing two WojtekThrottleSchedulers in pararell could be risky becase of log file (TODO)
   log_file_ << "tick," << "x," << "turbo," << "switch," << "a," << "v," << "dir," << "rad," << "piece_no," << "start_lane," << "end_land," << "schedule_time_limit," << "schedule_time," << "initial_schedule_safe," << "nodes_visited," << "leafs_visited," << "unsafe_cuts," << "ub_cuts," << "solution_improvements," << "schedule," << "predicted_angles," << "0_throttle_predictions" << std::endl;
}

WojtekThrottleScheduler::~WojtekThrottleScheduler() {
    log_file_.close();
}

void WojtekThrottleScheduler::Schedule(const game::CarState& state, int game_tick, const utils::Deadline& deadline) {
  last_time_limit_ = deadline.GetDurationToExpire().count() * 1000.0;
  utils::StopWatch stopwatch;

  // We want to use the last best_schedule_ if possible. Generally always 
  // tick_diff should always be 1, but I take extra care if 
  // (for some unknown reason) this is not the case
  int tick_diff = game_tick - last_game_tick_;
  if (1 <= tick_diff && tick_diff <= 4) {
    for (int i = 0; i < tick_diff; ++i) {
      best_schedule_.ShiftLeftFillSafe(state);
    }
  } else {
    // We will still try to use the old schedule, but, it could be not safe.
    // This will be checked subsequently
  }
  // Must do it, because we could have unpredicted turbo/switches, etc. ahead
  best_schedule_.UpdateDistance(state); 

  // Initial schedule should be safe normally. Not safe only if future predictions
  // have changed
  initial_schedule_safe_ = best_schedule_.IsSafe(state);
  if (!initial_schedule_safe_) {
    best_schedule_.Reset(state);
  }

  branch_and_bound_.Improve(state, best_schedule_, deadline);
  local_improver_.Improve(state, best_schedule_, 0.1, deadline);

  // Once again, just to be 200% sure check for safety
  if (!best_schedule_.IsSafe(state)) {
    std::cerr << "Schedule is not safe. This should not happen" << std::endl;
    best_schedule_.Reset(state);
  }

  last_game_tick_ = game_tick;
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
    PrintSchedule(state, best_schedule_, std::min(horizon_, 20));
  }

  game::Piece piece = race_.track().pieces()[state.position().piece()];
  log_file_ << last_game_tick_
            << ',' << throttle()
            << ',' << state.turbo_state().is_on() 
            << ',' << (int)state.switch_state() 
            << ',' << state.position().angle() 
            << ',' << state.velocity()
            << ',' << -sgn(piece.angle())
            << ',' << piece.radius() 
            << ',' << state.position().piece()
            << ',' << state.position().start_lane()
            << ',' << state.position().end_lane()
            << ',' << last_time_limit_
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
  log_file_ << ',';
  CarState next = state;
  for (int i = 0; i < best_schedule_.size(); ++i) {
    next = car_tracker_.Predict(next, game::Command(best_schedule_[i]));
    log_file_ << std::setprecision(2) << next.position().angle() << ' ';
  }
  log_file_ << ',';
  next = state;
  for (int i = 0; i < best_schedule_.size(); ++i) {
    next = car_tracker_.Predict(next, game::Command(0));
    log_file_ << std::setprecision(2) << next.position().angle() << ' ';
  }
  log_file_ << std::endl;
  log_file_.flush();
}

}  // namespace schedulers
