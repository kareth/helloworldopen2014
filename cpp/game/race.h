#ifndef CPP_GAME_RACE_H_
#define CPP_GAME_RACE_H_

#include <iostream>
#include <vector>

#include "jsoncons/json.hpp"
#include "game/car.h"
#include "game/track.h"

namespace game {

class Race {
 public:
  // The input json should point to the "race" part of the "gameInit"
  // command. E.g.
  //
  // {
  //   "cars": [],
  //   "raceSession": {},
  //   "track": {}
  // }
  void ParseFromJson(const jsoncons::json& data);

  const int laps() const { return laps_; }
  const int max_lap_time_ms() const { return max_lap_time_ms_; }
  const bool quick_race() const { return quick_race_; }

  const Track& track() const { return track_; }
  const std::vector<Car> cars() const { return cars_; }

  bool qualification_phase() const { return duration_ != 0; }
  bool race_phase() const { return duration_ == 0; }

  const jsoncons::json& ToJson() const { return data_; }

 private:
  int laps_ = 0;
  int max_lap_time_ms_ = 0;
  bool quick_race_ = false;

  int duration_ = 0;

  Track track_;
  std::vector<Car> cars_;

  // JSON representation for simulator.
  jsoncons::json data_;
};

}  // namespace game

#endif  // CPP_GAME_RACE_H_
