#ifndef CPP_SCHEDULERS_LOCAL_IMPROVER_H_
#define CPP_SCHEDULERS_LOCAL_IMPROVER_H_

#include "schedulers/schedule.h"
#include "game/car_predictor.h"
#include "utils/deadline.h"

namespace schedulers {

class LocalImprover {
 public:
  LocalImprover() {}
  bool Improve(const game::CarState& state, Sched& schedule, double step, const utils::Deadline& deadline, double distance_to_switch, double last_throttle);

 private:
  bool ImproveOne(const game::CarState& state, Sched& schedule, int idx, double step, double distance_to_switch, double last_throttle);
};

} // namespace

#endif
