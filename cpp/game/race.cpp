#include "game/race.h"

using jsoncons::json;

namespace game {

void Race::ParseFromJson(const json& data) {
  track_.ParseFromJson(data["track"]);

  laps_ = data["raceSession"]["laps"].as_int();
  max_lap_time_ms_ = data["raceSession"]["maxLapTimeMs"].as_int();
  quick_race_ = data["raceSession"]["quickRace"].as_bool();

  for (auto it = data["cars"].begin_elements(); it != data["cars"].end_elements(); ++it) {
    cars_.push_back(Car());
    cars_.back().ParseFromJson(*it);
  }
}

}  // namespace game
