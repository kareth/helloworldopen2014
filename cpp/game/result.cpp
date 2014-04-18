#include "game/result.h"

namespace game {

Result::Result() : color_() {
}

std::string Result::ParseFromJson(const jsoncons::json& data) {
  color_ = data["car"]["color"].as_string();

  auto& lap_time = data["lapTime"];
  lap_ = lap_time["lap"].as_int();
  lap_time_ = lap_time["millis"].as_int();

  auto& race_time = data["raceTime"];
  laps_ = race_time["laps"].as_int();
  race_time_ = race_time["millis"].as_int();

  auto& ranking = data["ranking"];
  fastest_lap_ = ranking["fastestLap"].as_int();
  rank_ = ranking["overall"].as_int();
}

}
