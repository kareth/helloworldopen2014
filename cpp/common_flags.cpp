#include "gflags/gflags.h"

DEFINE_string(race_id, "", "");
DEFINE_double(safe_angle, 60.0, "");
DEFINE_int32(handicap, 0, "");
DEFINE_bool(check_if_safe_ahead, true, "");
DEFINE_int32(answer_time, 10, "Time limit for answer in ms");


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
