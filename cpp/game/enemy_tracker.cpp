#include "game/enemy_tracker.h"

using game::Position;
using game::Race;

namespace game {

EnemyTracker::EnemyTracker(const Race& race, const std::string& color)
  : race_(race), color_(color) {


}

void EnemyTracker::Record(const std::map<std::string, Position>& positions) {
  for (auto& p : positions) {
    if (p.fist == color_) continue;

    if (indexes_.[p.first] == nullptr) {
      indexes_[p.first] = enemies_.size();
      enemies_.resize(enemies_.size() + 1);
    }

    // TODO use state somehow...
    RecordEnemy(indexes_[p.first], p.second);
  }

  sort(cars_ahead_.begin(), cars_ahead_.end(), [](const EnemyInfo* a, const EnemyInfo* b)
      { return a->distance() < b->distance(); });
  sort(cars_behind_.begin(), cars_behind_.end(), [](const EnemyInfo* a, const EnemyInfo* b)
      { return a->distance() > b->distance(); });
}

void EnemyTracker::RecordEnemy(int index, const Position& position) {
  UpdateSpeedStats(index, position);
  // TODO update EnemyInfo

}

void UpdateSpeedStats(int index, const game::Position& position) {
  // TODO Update piece_speeds
  // TODO Update lap_speeds
}

void EnemyTracker::RecordLapTime(const std::string& color, int time) {
  int index = indexes_[color];

  if (lap_times_[index] == nullptr)
    lap_times_[index] = {};
  lap_times_[index].push_back(time);
}

}  // namespace game
