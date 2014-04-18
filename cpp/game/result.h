#ifndef CPP_GAME_RESULT_H_
#define CPP_GAME_RESULT_H_

#include <string>
#include <iostream>
#include "jsoncons/json.hpp"


namespace game {

class Result {
 public:
  Result();

  std::string ParseFromJson(const jsoncons::json& data);

  const std::string& color() const { return color_; }
  int lap() const { return lap_; }
  int laps() const { return laps_; }

  int lap_time() const { return lap_time_; }
  int race_time() const { return race_time_; }

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
