#ifndef CPP_GAME_LANE_LENGTH_MODEL_H_
#define CPP_GAME_LANE_LENGTH_MODEL_H_

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <algorithm>

#include "game/gauss.h"
#include "game/error_tracker.h"

namespace game {

class LaneLengthModel {
 public:
  LaneLengthModel(Track* track) : track_(track) {}

 private:
  Track* track_;

  map<double, double> switch_on_straight_length_;
  map<pair<double, double>, double switch_on_turn_length_;
};

}  // namespace game

#endif  // CPP_GAME_VELOCITY_MODEL_H_
