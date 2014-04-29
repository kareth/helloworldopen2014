#include "gflags/gflags.h"

DEFINE_string(race_id, "", "");
DEFINE_bool(continuous_integration, true, "");
DEFINE_double(safe_angle, 60.0, "");
DEFINE_int32(handicap, 0, "");
DEFINE_bool(check_if_safe_ahead, true, "");
DEFINE_int32(answer_time, 10, "Time limit for answer in ms");


DEFINE_bool(bump_with_turbo, true, "kamikazebumps");
DEFINE_bool(defend_turbo_bump, false, "Defend against people that have bump_with_turbo");
