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

  if (straights_.size() == 0) {
    should_fire_now_ = false;
    return;
  }

  // TODO its kind of greedy,
  // first of all - we shouldnt count just straights
  // second, it has to make some kind of decisions based on turbo freq

  // Longest overall
  //
  if (strategy_ == Strategy::kOptimizeRace
      || strategy_ == Strategy::kOptimizeNextLap) {
    if (IsOnStraightBegin(state, straights_[0])) {
      should_fire_now_ = true;
    } else if (CanFireBeforeStraight(state, straights_[0])) {
      should_fire_now_ = true;
    }
  } else if (strategy_ == Strategy::kOptimizeCurrentLap) {
    // Longest in between now and lap end
    for (int i = 0; i < straights_.size(); i++) {
      if (IsOnStraightBegin(state, straights_[i])) {
        should_fire_now_ = true;
      } else if (CanFireBeforeStraight(state, straights_[i])) {
        should_fire_now_ = true;
      } if (straights_[i].from() > state.position().piece()) {
        return;
      }
    }
  }

}

bool GreedyTurboScheduler::IsOnStraightBegin(const game::CarState& state, const Straight& straight) {
  game::Position begin_position(straight.from(), 0);
  begin_position.set_start_lane(state.position().start_lane());
  begin_position.set_end_lane(state.position().start_lane());

  double distance = car_tracker_.DistanceBetween(begin_position, state.position());
  return distance < 100.0;
}

bool GreedyTurboScheduler::CanFireBeforeStraight(const game::CarState& state, const Straight& straight) {
  if (IsOnStraightBegin(state, straight))
    return true;

  if (state.position().piece() > straight.from())
    return false;

  // W teoriinie trzeba samych jedynek...
  if (state.position().piece() <= straight.from() &&
      state.position().piece() >= straight.from() - 2) {
    auto s = car_tracker_.Predict(state, game::Command(game::TurboToggle::kToggleOn));
    int max_ticks = 80;
    while (max_ticks-- > 0 && !IsOnStraightBegin(s, straight)) {
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
  for (int i = 0; i < pieces.size() * 2; i++) {
    int piece = i % pieces.size();

    if (pieces[piece].type() == game::PieceType::kStraight ||
        (pieces[piece].radius() >= 150 && from != -1)) {
      if (from == -1)
        from = piece;

      game::Position position(piece, 0);
      double piece_length = car_tracker_.lane_length_model().Length(position);
      length += piece_length;
    } else {
      if (from != -1) {
        int to = (piece + pieces.size() - 1) % pieces.size();
        straights_.push_back(Straight(length, from, to));
        length = 0;
        from = -1;
      }

      // Finish if we already calculated the straight that has passed across
      // the start line
      if (i >= pieces.size() - 1)
        break;
    }
  }

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
