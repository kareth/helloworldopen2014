#include "game/bump_detector.h"

namespace game {

BumpDetector::BumpDetector(CarTracker& car_tracker, const Race& race)
  : car_tracker_(car_tracker), race_(race) {
}

void BumpDetector::Record(const std::vector<EnemyTracker>& enemies, const std::map<std::string, Position>& positions, int game_tick) {
  bumps_.clear();
  const double kCarLength = race_.cars()[0].length();
  for (auto& a : enemies) {
    if (positions.find(a.color()) == positions.end()) continue;
    for (auto& b : enemies) {
      if (positions.find(b.color()) == positions.end()) continue;

      auto& a_pos = positions.at(a.color());
      auto& b_pos = positions.at(b.color());

      if (a.color() == b.color()) continue;

      if (a.is_dead() || b.is_dead())
        continue;

      // NOTE - if this logic changes, update also CarTracker!!!

      double distance = car_tracker_.DistanceBetween(a_pos, b_pos);
      // We need to compare start_lane and end_lane, as for switches,
      // bump only occurs if those 2 params are equal for both cars
      if (distance <= kCarLength + 1e-9 &&
          a_pos.start_lane() == b_pos.start_lane() &&
          a_pos.end_lane() == b_pos.end_lane()) {
        bumps_.push_back({ a.color(), b.color() });

        history_[{ a.color(), b.color() }].push_back(game_tick);
        printf("Bump detected! %s %s\n", a.color().c_str(), b.color().c_str());
      }
    }
  }
}

bool BumpDetector::BumpOccured(const std::string& color, const std::string& color2) {
  for (auto& b : bumps_) {
    if ((b.first == color && b.second == color2) ||
        (b.second == color && b.first == color2))
      return true;
  }
  return false;
}

int BumpDetector::BumpsBetween(const std::string& color, const std::string& color2, int time_span) {
  int res = 0;
  for (int i = int(history_[{ color, color2 }].size()) - 1; i >= 0; i--) {
    int t = history_[{color, color2}][i];
    if (t > last_tick_ - time_span)
      res++;
    else
      break;
  }
  return res;
}

}  // namespace game
