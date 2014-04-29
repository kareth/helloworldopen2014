#include "schedulers/greedy_turbo_scheduler.h"

namespace schedulers {

GreedyTurboScheduler::GreedyTurboScheduler(const game::Race& race,
                      game::CarTracker& car_tracker)
  : race_(race), car_tracker_(car_tracker), should_fire_now_(false) {
  FindLongestStraights();
}

// Returns if should we use turbo
bool GreedyTurboScheduler::ShouldFireTurbo() {
  return should_fire_now_;
}

// Makes decision on turbo usage
void GreedyTurboScheduler::Schedule(const game::CarState& state) {
  if (!state.turbo_state().available() ||
       state.turbo_state().is_on()) {
    should_fire_now_ = false;
    return;
  }

  // TODO its kind of greedy,
  // first of all - we shouldnt count just straights
  // second, it has to make some kind of decisions based on turbo freq

  if (strategy_ == Strategy::kOptimizeRace) {
    // Longest overall
    if (state.position().piece() == straights_[0].from()) {
      should_fire_now_ = true;
    } else if (CanFireBeforeStraight(state, straights_[0])) {
      should_fire_now_ = true;
    }
  } else if (strategy_ == Strategy::kOptimizeCurrentLap) {
    // Longest in between now and lap end
    for (auto& s : straights_) {
      if (s.from() == state.position().piece()) {
        should_fire_now_ = true;
      } else if (CanFireBeforeStraight(state, s)) {
        should_fire_now_ = true;
      } if (s.from() > state.position().piece()) {
        return;
      }
    }
  } else if (strategy_ == Strategy::kOptimizeNextLap) {
    // Give him best speed for next lap
    // TODO not optimal, just taking last piece
    // TODO commenting this wins usa
    auto last = &straights_[0];
    for (auto& s : straights_)
      if (s.from() > last->from())
        last = &s;

    // If connected to 0
    if (last->to() == race_.track().pieces().size() - 1)
      if (state.position().piece() == last->from())
        should_fire_now_ = true;
  }
}

bool GreedyTurboScheduler::CanFireBeforeStraight(const game::CarState& state, const Straight& straight) {
  if (state.position().piece() == straight.from())
    return true;

  if (state.position().piece() > straight.from())
    return false;

  // W teoriinie trzeba samych jedynek...
  if (state.position().piece() <= straight.from() &&
      state.position().piece() >= straight.from() - 2) {
    auto s = car_tracker_.Predict(state, game::Command(game::TurboToggle::kToggleOn));
    int max_ticks = 80;
    while (max_ticks-- > 0 && s.position().piece() != straight.from()) {
      if (!car_tracker_.crash_model().IsSafe(s.position().angle()))
        return false;
      s = car_tracker_.Predict(s, game::Command(1));
    }
    return car_tracker_.IsSafe(s);
  }
  return false;
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
      if (a.length() - b.length() > 1e-9) return true;
      if (b.length() - a.length() > 1e-9) return false;
      return a.from() < b.from(); } );
}

// Prepare for overtake
void GreedyTurboScheduler::Overtake(const string& color) {
  printf("Feature not implemented.\n");
}

void GreedyTurboScheduler::TurboUsed() {
  should_fire_now_ = false;
}

}  // namespace schedulers
