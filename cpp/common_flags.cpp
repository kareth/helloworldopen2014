#include "gflags/gflags.h"

DEFINE_string(race_id, "", "");
DEFINE_double(safe_angle, 60.0, "");
DEFINE_int32(handicap, 0, "");
DEFINE_bool(check_if_safe_ahead, true, "");
DEFINE_int32(answer_time, 5000, "Time limit for answer in ms");

DEFINE_bool(disable_attack, false, "");
DEFINE_bool(bump_with_turbo, true, "kamikazebumps");
DEFINE_bool(defend_turbo_bump, false, "Defend against people that have bump_with_turbo");

DEFINE_string(switch_scheduler, "ShortestPathSwitchScheduler", "");
DEFINE_string(throttle_scheduler, "WojtekThrottleScheduler", "");

DEFINE_bool(print_models, false, "If true, all models will print summary at destruction");
DEFINE_bool(continuous_integration, true, "");

DEFINE_bool(read_switch_models, true, "If true, switch models (length + radius) are read from files");
DEFINE_bool(write_switch_models, false, "If true, switch models (length + radius) are written to files");

DEFINE_bool(log_overtaking, false, "log overtaking scores");
DEFINE_bool(always_switch, false, "");
DEFINE_bool(force_safe_angle, false, "");

DEFINE_bool(log_simulator_csv, false, "Log in simulator to data.csv");

// Simulator
DEFINE_int32(laps, 3, "Number of laps");

DEFINE_double(overtake_treshold, 0.9, "Ratio of velocity to overtake guy");
DEFINE_double(turbo_bump_treshold, 0.94, "Ratio of best laps");

DEFINE_bool(fastbanana_mode, false, "");
