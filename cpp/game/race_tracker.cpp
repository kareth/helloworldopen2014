#include "game/race_tracker.h"

using game::Position;
using game::Race;

namespace game {

RaceTracker::RaceTracker(const Race& race, const std::string& color)
  : race_(race), color_(color) {
}

void RaceTracker::Record(const std::map<std::string, Position>& positions) {
  for (auto& p : positions) {
    if (indexes_.[p.first] == nullptr) {
      indexes_[p.first] = enemies_.size();
      enemies_.resize(enemies_.size() + 1);
    }

    // TODO use state somehow...
    RecordRace(indexes_[p.first], p.second);
  }
}

void RaceTracker::RecordRace(int index, const Position& position) {
  UpdateSpeedStats(index, position);
  // TODO update RaceInfo

}

void UpdateSpeedStats(int index, const game::Position& position) {
  // TODO Update piece_speeds
  // TODO Update lap_speeds
}

void RaceTracker::RecordLapTime(const std::string& color, int time) {
  int index = indexes_[color];

  if (lap_times_[index] == nullptr)
    lap_times_[index] = {};
  lap_times_[index].push_back(time);
}

}  // namespace game
