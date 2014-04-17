#ifndef CPP_GAME_CAR_H_
#define CPP_GAME_CAR_H_

#include <iostream>
#include <string>

#include "jsoncons/json.hpp"

using std::string;
using std::vector;

namespace game {

class Car {
 public:
  const string name() const { return name_; }
  const string color() const { return color_; }
  const double length() const { return length_; }
  const double width() const { return width_; }
  const double guide_flag_position() const { return guide_flag_position_; }

  // The input json should point to the "cars" part of the "gameInit"
  // command. E.g.
  //
  // {
  //   "dimensions": {},
  //   "id": {},
  // }
  void ParseFromJson(const jsoncons::json& data);

 private:
  string name_;
  string color_;
  double length_;
  double width_;
  double guide_flag_position_;
};

}  // namespace game

#endif  // CPP_GAME_CAR_H_
