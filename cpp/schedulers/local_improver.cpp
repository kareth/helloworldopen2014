#include "schedulers/local_improver.h"

namespace schedulers {

bool LocalImprover::ImproveOne(const game::CarState& state, Sched& schedule, int idx, double step, double distance_to_switch, double last_throttle) {
  bool improved = false;
  while (schedule[idx] + step <= 1.0) {
    schedule[idx] += step;
    if (!schedule.IsSafe(state, distance_to_switch, last_throttle)) {
      schedule[idx] -= step;
      break;
    }
    // If throttle was increased there has to be distance improvement
    improved = true;
  }
  return improved;
}

bool LocalImprover::Improve(const game::CarState& state, Sched& schedule, double step, const utils::Deadline& deadline, double distance_to_switch, double last_throttle) {
  //FIXME: switches
  bool improved = false;
  for (int i=0; i<schedule.size(); ++i) {
    if (deadline.HasExpired())
      break;
    improved = ImproveOne(state, schedule, i, step, distance_to_switch, last_throttle);
  }
  if (improved)
    schedule.UpdateDistance(state);
  return improved;
}

} // namespace
