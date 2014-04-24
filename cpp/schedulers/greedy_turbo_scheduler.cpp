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

  // TODO its kind of greedy,
  // first of all - we shouldnt count just straights
  // second, it has to make some kind of decisions based on turbo freq

  if (strategy_ == Strategy::kOptimizeRace) {
    // Longest overall
    if (state.position().piece() == straights_[0].from()) {
      should_fire_now_ = true;
    }
  } else if (strategy_ == Strategy::kOptimizeCurrentLap) {
    // Longest in between now and lap end
    for (auto& s : straights_) {
      if (s.from() == state.position().piece())
        should_fire_now_ = true;
      if (s.from() > state.position().piece())
        return;
    }
  } else if (strategy_ == Strategy::kOptimizeNextLap) {
    // Give him best speed for next lap
    // TODO not optimal, just taking last piece
    auto last = &straights_[0];
    for (auto& s : straights_)
      if (s.from() > last->from())
        last = &s;

    // If connected to 0
    if (last->to() == race_.track().pieces().size() - 1)
      if (last->from() == state.position().piece())
        should_fire_now_ = true;
  }
}

void GreedyTurboScheduler::FindLongestStraights() {
  const auto& pieces = race_.track().pieces();

  int from = -1;
  double length = 0;
  for (int i = 0; i < pieces.size(); i++) {
    if (pieces[i].type() == game::PieceType::kStraight) {
      if (from == -1)
        from = i;
      length += pieces[i].length();
    } else {
      if (from != -1) {
        straights_.push_back(Straight(length, from, i - 1));
        length = 0;
        from = -1;
      }
    }
  }
  if (from != -1)
    straights_.push_back(Straight(length, from, pieces.size() - 1));

  sort(straights_.begin(), straights_.end(), [](const Straight& a, const Straight& b) {
      if (a.length() != b.length())
        return a.length() > b.length();
      return a.from() < b.from(); } );
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
