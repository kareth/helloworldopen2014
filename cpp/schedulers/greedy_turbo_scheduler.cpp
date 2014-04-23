#include "schedulers/greedy_turbo_scheduler.h"

namespace schedulers {

GreedyTurboScheduler::GreedyTurboScheduler(const game::Race& race,
                      game::CarTracker& car_tracker)
  : race_(race), car_tracker_(car_tracker), turbo_available_(false),
    should_fire_now_(false) {
}

// Returns if should we use turbo
bool GreedyTurboScheduler::ShouldFireTurbo() {
  return should_fire_now_;
}

// Makes decision on turbo usage
void GreedyTurboScheduler::Schedule(const game::CarState& state) {
  if (!turbo_available_) {
    should_fire_now_ = false;
    return;
  }

  // TODO(kareth) strategies
  // TODO check if its safe to fire turbo now.
  if (state.position().lap() == race_.laps() - 1 &&
      race_.track().IsLastStraight(state.position()))
    should_fire_now_ = true;
}

// Prepare for overtake
void GreedyTurboScheduler::Overtake(const string& color) {
  printf("Feature not implemented.\n");
}

void GreedyTurboScheduler::NewTurbo(const game::Turbo& turbo) {
  turbo_available_ = true;
  turbo_ = turbo;
}

void GreedyTurboScheduler::TurboUsed() {
  turbo_available_ = false;
  should_fire_now_ = false;
}

}  // namespace schedulers
