#include "physics/speed_tracker.h"
#include <cstdio>
using std::string;
using std::vector;
using std::map;

namespace physics {

SpeedTracker::SpeedTracker(const game::Race& race)
  : race_(race), speeds_(), last_positions_() {
}

void SpeedTracker::Update(const map<string, game::Position>& positions) {
  if (last_positions_.size() == 0) {
    last_positions_ = positions;
    for (auto& p : positions)
      speeds_[p.first] = 0;
    return;
  }

  for (auto& p : positions) {
    auto& color = p.first;
    auto& position = p.second;
    auto& last_position = last_positions_[color];

    if (position.piece() == last_position.piece())
      speeds_[color] = position.piece_distance() - last_position.piece_distance();

    last_positions_[color] = position;
  }
}

}  // namespace physics
