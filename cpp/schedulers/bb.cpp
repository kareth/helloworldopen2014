#include "schedulers/bb.h"

namespace schedulers {

BranchAndBound::BranchAndBound(game::CarTracker* car_tracker, int horizon) 
  : horizon_(horizon), car_tracker_(car_tracker) 
{ }

void BranchAndBound::Improve(Sched& schedule) {
  //TODO
}

}
