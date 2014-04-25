#ifndef CPP_GAME_LANE_LENGTH_MODEL_H_
#define CPP_GAME_LANE_LENGTH_MODEL_H_

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <algorithm>

#include "game/track.h"

namespace game {

class LaneLengthModel {
 public:
  // Track has to outlive this model.
  LaneLengthModel(const Track* track) : track_(track) {}

  // perfect - return if the returned length is on 100% correct
  double Length(const Position& position, bool* perfect=nullptr) const;

  void Record(const Position& previous, const Position& current, double predicted_velocity);

 private:
  const Track* track_;

  std::map<std::pair<double, double>, double> switch_on_straight_length_;
  std::map<std::pair<double, double>, double> switch_on_turn_length_;
};

}  // namespace game

#endif  // CPP_GAME_VELOCITY_MODEL_H_
