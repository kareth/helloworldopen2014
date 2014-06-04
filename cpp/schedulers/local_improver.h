#ifndef CPP_SCHEDULERS_LOCAL_IMPROVER_H_
#define CPP_SCHEDULERS_LOCAL_IMPROVER_H_

#include "schedulers/schedule.h"
#include "game/car_predictor.h"

namespace schedulers {

class LocalImprover {
 public:
  LocalImprover() {}
  bool ImproveOne(const game::CarState& state, Sched& schedule, int idx, double step);
  bool Improve(const game::CarState& state, Sched& schedule, double step);
};

} // namespace

#endif
