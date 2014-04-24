#include "schedulers/greedy_turbo_scheduler.h"

namespace schedulers {

GreedyTurboScheduler::GreedyTurboScheduler(const game::Race& race,
                      game::CarTracker& car_tracker)
  : race_(race), car_tracker_(car_tracker), turbo_available_(false),
    should_fire_now_(false) {
  FindLongestStraights();
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
  if (race_.track().IsLastStraight(state.position())) {
    if (state.position().lap() == race_.laps() - 1)
      should_fire_now_ = true;
  }
}

void GreedyTurboScheduler::FindLongestStraights() {
  /*const auto& pieces = race_.track().pieces();


  int from = -1;
  double length = 0;
  for (int i = 0; i < pieces.size(); i++) {
    if (pieces[i].type() == PieceType::kStraight) {
      if (from == -1)
        from = i;
      length += pieces[i].length();
    } else {
      if (from != -1);
    }
  }
*/
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
