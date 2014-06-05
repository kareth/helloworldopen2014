#include "game/race.h"

using jsoncons::json;

namespace game {

void Race::ParseFromJson(const json& data) {
  track_.ParseFromJson(data["track"]);

  const auto& race_session = data.get("raceSession", json());
  laps_ = race_session.get("laps", json(0)).as_int();
  max_lap_time_ms_ = race_session.get("maxLapTimeMs", json(0)).as_int();
  quick_race_ = race_session.get("quickRace", json(false)).as_bool();

  duration_ = race_session.get("durationMs", json(0)).as_int();

  for (auto it = data["cars"].begin_elements(); it != data["cars"].end_elements(); ++it) {
    cars_.push_back(Car());
    cars_.back().ParseFromJson(*it);
  }

  data_ = data;
}

}  // namespace game
