#ifndef CPP_GAME_RADIUS_MODEL_H_
#define CPP_GAME_RADIUS_MODEL_H_

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <algorithm>

#include "game/error_tracker.h"
#include "game/gauss.h"
#include "game/position.h"
#include "game/race.h"
#include "game/simplex.h"

namespace game {

class SwitchRadiusModel {
 public:
  // Returns 0 for straight line.
  double Radius(double piece_distance) const;

  void Record(double piece_distance, double radius);

  bool IsReady() const {
    return ready_;
  }
 private:
  vector<vector<double>> model_;
  vector<double> b_;
  vector<double> x_;

  bool ready_;
};

class RadiusModel {
 public:
  RadiusModel(const Track* track) : track_(track) {
    file_.open("bin/switch.csv", std::ios::out | std::ios::app);
    file_ << "piece,start_lane,end_lane,piece_distance,radius" << std::endl;
  }

  void Record(const Position& position, double radius);

  // Returns 0 for straight line.
  double Radius(const Position& position);

 private:
  // (piece, start_lane) -> model
  std::map<std::pair<int, int>, SwitchRadiusModel> models_;
  std::ofstream file_;

  const Track* track_;
};

}  // namespace game

#endif  // CPP_GAME_RADIUS_MODEL_H_
