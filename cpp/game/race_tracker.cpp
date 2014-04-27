#include "game/race_tracker.h"

using game::Position;
using game::Race;

namespace game {

RaceTracker::RaceTracker(game::CarTracker& car_tracker,
          const game::Race& race, const std::string& color)
  : car_tracker_(car_tracker), race_(race), color_(color) {
}

void RaceTracker::Record(const std::map<std::string, Position>& positions) {
  for (auto& p : positions) {
    if (indexes_.find(p.first) == indexes_.end()) {
      indexes_[p.first] = enemies_.size();
      enemies_.push_back(EnemyTracker(car_tracker_, race_,  p.first, p.second));
    } else {
      enemies_[indexes_[p.first]].RecordPosition(p.second);
    }
  }
}

void RaceTracker::RecordLapTime(const std::string& color, int time) {
  if (indexes_.find(color) == indexes_.end())
    return;

  enemies_[indexes_[color]].RecordLapTime(time);
}

void RaceTracker::RecordCrash(const std::string& color) {
  if (indexes_.find(color) == indexes_.end())
    return;

  enemies_[indexes_[color]].RecordCrash();
}

// TODO test
std::vector<std::string> RaceTracker::CarsBetween(int from, int to, int lane) {
  std::vector<std::string> result;
  for (auto& i : indexes_) {
    if (i.first == color_) continue;
    if (race_.track().IsBetween(enemies_[i.second].state().position(), from, to) &&
        enemies_[i.second].state().position().end_lane() == lane)
      result.push_back(i.first);
  }
  return result;
}

std::vector<std::string> RaceTracker::PredictedCarsBetween(int from, int to, int lane, int time) {
  return CarsBetween(from, to, lane);
/*  std::vector<std::string> result;
  for (auto& i : indexes_) {
    if (i.first == color_) continue;
    if (enemies_[i.second].state().position().end_lane() == lane()) {
      auto position = enemies_[i.second].PositionAfter(time);

      if (race_.track().IsBetween(position, from, to))
        result.push_back(i.first);
    }
  }
  return result;*/
}

/* Position RaceTracker::BumpPosition(const std::string& color) {
  int index = indexes_[color];

  // TODO optimize:D
  for (int i = 0; i < 100; i++) {
    auto me = enemies_[indexes_[color_]].PositionAfterTime(i);
    auto he = enemies_[indexes_[color]].PositionAfterTime(i);

    if ((me.piece() == he.piece() && me.piece_distance() > he.piece_distance())
        || (me.piece() + 1) % race_.track().pieces().size() == he.piece())
      return me;
  }
  // This shouldnt reach here
  return enemies_[indexes_[color_]].state().position();
} */

}  // namespace game
