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
  LaneLengthModel(const Track* track);
  ~LaneLengthModel();

  // perfect - return if the returned length is on 100% correct
  double Length(const Position& position, bool* perfect=nullptr) const;

  void Record(const Position& previous, const Position& current, double predicted_velocity);

 private:
  void LoadSwitchLengths();
  void SaveSwitchLengths();

  const Track* track_;

  // {length, width} => switch_length
  std::map<std::pair<double, double>, double> switch_on_straight_length_;
  // {start_radius, end_radius, angle} => switch_length
  std::map<std::tuple<double, double, double>, double> switch_on_turn_length_;
};

}  // namespace game

#endif  // CPP_GAME_VELOCITY_MODEL_H_
