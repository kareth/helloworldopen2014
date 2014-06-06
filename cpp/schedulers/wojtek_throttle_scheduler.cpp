#include "schedulers/wojtek_throttle_scheduler.h"

#include <chrono>
#include <cstdio>
#include <cmath>

#include "gflags/gflags.h"
#include "utils/stopwatch.h"

DEFINE_bool(log_schedule, false, "Log schedule on stdout");
DEFINE_bool(log_wojtek_to_file, false, "Log WojtekSchedulerData to csv file");

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
    game::CarTracker& car_tracker, const vector<int>& groups, bool log_to_csv)
  : race_(race), car_tracker_(car_tracker), 
    groups_(groups),
    horizon_(std::accumulate(groups.begin(), groups.end(), 0)),
    best_schedule_(&car_tracker, horizon_),
    branch_and_bound_(&car_tracker, horizon_, groups_, values),
    log_to_csv_(log_to_csv)
{
  if (log_to_csv_) {
    log_file_.open("wojtek_data_log.csv", std::ofstream::out);
    log_file_ << "tick," << "x," << "turbo," << "switch," << "a," << "v," << "dir," << "rad," << "piece_no," << "start_lane," << "end_land," << "schedule_time_limit," << "schedule_time," << "initial_schedule_safe," << "nodes_visited," << "leafs_visited," << "unsafe_cuts," << "ub_cuts," << "solution_improvements," << "schedule," << "predicted_angles," << "0_throttle_predictions" << std::endl;
  }
}

WojtekThrottleScheduler::~WojtekThrottleScheduler() {
  if (log_to_csv_) {
    log_file_.close();
  }
}

bool WojtekThrottleScheduler::RepairInitialSchedule(const game::CarState& state, Sched& schedule,
    double distance_to_switch, double last_throttle) {
  if (distance_to_switch < 0) {
    // I can repair only switch-related issues (TODO)
    return false;
  }

  // Try to repair switch
  int ticks = schedule.GetTicksToTheRequiredSwitch(state, distance_to_switch);
  if (ticks < 0) {
    // Switch is too far away, so it not the problem in this Schedule
    return false;
  }

  // Try to inject the switch at three positions before the switch is due
  for (int tick = ticks; tick >= std::max(0, ticks - 2); --tick) {
    if (schedule.TryUpdateSwitchPosition(state, tick, distance_to_switch, last_throttle)) {
      schedule.UpdateDistance(state);
      return true;
    }
  }

  // If the above failed, try to 0.0 after the switch...
  for (int i = ticks; i < schedule.size(); ++i) {
    schedule[i] = 0.0;
  }

  // ... and once again try to inject the switch
  for (int tick = ticks; tick >= std::max(0, ticks - 2); --tick) {
    if (schedule.TryUpdateSwitchPosition(state, tick, distance_to_switch, last_throttle)) {
      schedule.UpdateDistance(state);
      return true;
    }
  }
  
  return false;
}

bool WojtekThrottleScheduler::Schedule(const game::CarState& state, int game_tick, 
        const utils::Deadline& deadline, double distance_to_switch, double last_throttle) {
  last_time_limit_ = deadline.GetDurationToExpire().count() * 1000.0;
  utils::StopWatch stopwatch;
  if (!log_to_csv_) printf("%f %f\n", distance_to_switch, last_throttle);

  // We want to use the last best_schedule_ if possible. Generally always 
  // tick_diff should always be 1, but I take extra care if 
  // (for some unknown reason) this is not the case
  if (!log_to_csv_) best_schedule_.Print();
  int tick_diff = game_tick - last_game_tick_;
  if (1 <= tick_diff && tick_diff <= 4) {
    for (int i = 0; i < tick_diff; ++i) {
      best_schedule_.ShiftLeftFillSafe(state, distance_to_switch, last_throttle);
    }
  } else {
    // We will still try to use the old schedule, but, it could be not safe.
    // This will be checked subsequently
  }
  // Must do it (never ever think of removing it!)
  best_schedule_.UpdateDistance(state); 
  if (!log_to_csv_) {
    best_schedule_.Print();
  }

  // Initial schedule should be safe normally. 
  // Not safe only if future predictions or switch or turbo decisions have changed
  initial_schedule_safe_ = best_schedule_.IsSafe(state, distance_to_switch, last_throttle);
  if (!log_to_csv_) printf("issafe %d\n", (int)initial_schedule_safe_);
  if (!initial_schedule_safe_) {
    bool repaired = RepairInitialSchedule(state, best_schedule_, distance_to_switch, last_throttle);
    if (!repaired) {
      //TODO: Create a good greedy one instead of resetting
      best_schedule_.Reset(state);
    }
  }

  branch_and_bound_.Improve(state, best_schedule_, deadline, distance_to_switch, last_throttle);
  local_improver_.Improve(state, best_schedule_, 0.1, deadline, distance_to_switch, last_throttle);

  bool is_safe = best_schedule_.IsSafe(state, distance_to_switch, last_throttle);
  if (!is_safe) {
  //  std::cerr << "Schedule is not safe. Generally, this should not happen" << std::endl;
  //  TODO: Make this warning turned off for Quick
    best_schedule_.Reset(state);
  }

  last_game_tick_ = game_tick;
  last_schedule_time_ = stopwatch.elapsed();
  Log(state);
  if (!log_to_csv_)
      printf("finished issafe %d\n",is_safe);
  if (!log_to_csv_) best_schedule_.Print();
  return is_safe;
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

bool WojtekThrottleScheduler::TimeToSwitch(int game_tick) {
  return best_schedule_.switch_position() == 0;
}

void WojtekThrottleScheduler::Log(const game::CarState& state) {
  if (FLAGS_log_schedule) {
    PrintSchedule(state, best_schedule_, std::min(horizon_, 20));
  }

  if (log_to_csv_) {
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
  }
}

}  // namespace schedulers
