#ifndef CPP_GAME_RESULT_H_
#define CPP_GAME_RESULT_H_

#include <string>
#include <iostream>
#include "jsoncons/json.hpp"

namespace game {

class Result {
 public:
  Result();

  void ParseFromJson(const jsoncons::json& data);

  const std::string& color() const { return color_; }

  // The current lap number.
  int lap() const { return lap_; }

  // Number of laps that we drove.
  int laps() const { return laps_; }

  // Returns lap time in milliseconds (1 tick = 100/6 milliseconds).
  int lap_time() const { return lap_time_; }
  int race_time() const { return race_time_; }

  // I assume those are ranking:
  // - which place do we have based on lap time,
  // - which place do we have in the race.
  int fastest_lap() const { return fastest_lap_; }
  int rank() const { return rank_; }

 private:
  std::string color_;
  int lap_;
  int laps_;
  int fastest_lap_;
  int rank_;
  int lap_time_;
  int race_time_;
};

}  // namespace game

#endif  // CPP_GAME_RESULT_H_
