#ifndef CPP_GAME_RADIUS_MODEL_H_
#define CPP_GAME_RADIUS_MODEL_H_

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <set>
#include <algorithm>

#include "game/error_tracker.h"
#include "game/gauss.h"
#include "game/position.h"
#include "game/race.h"
#include "game/simplex.h"
#include "gflags/gflags.h"

DECLARE_string(race_id);

namespace game {

class SwitchRadiusModel {
 public:
  SwitchRadiusModel(int piece, int start_lane)
    : piece_(piece), start_lane_(start_lane) {}

  ~SwitchRadiusModel() {
    std::cout << "==== Switch Radius Model ====" << std::endl;
    std::cout << "piece: " << piece_ << " start_lane: " << start_lane_ << std::endl;
    if (IsReady()) {
      for (int i = 0; i < x_.size(); i++)
        std::cout << std::setprecision(20) << "x" << i <<": " << x_[i] << " ";
      std::cout << std::endl;

      std::cout << "Model trained using: " << std::endl;
      for (int i = 0; i < model_.size(); ++i) {
        for (int j = 0; j < model_[i].size(); ++j) std::cout << std::setprecision(20) << model_[i][j] << " ";
        std::cout << std::endl;
      }
    }
    std::cout << std::endl;
  }

  // Returns 0 for straight line.
  double Radius(double piece_distance) const;

  void Record(double piece_distance, double radius);

  bool IsReady() const {
    return ready_;
  }

 private:
  bool EnoughData();

  vector<vector<double>> model_;
  vector<double> b_;
  vector<double> x_;

  // (piece_distance, radius)
  vector<std::pair<double, double> > data_;

  int piece_;
  int start_lane_;

  bool ready_ = false;
};

class RadiusModel {
 public:
  RadiusModel(const Track* track) : track_(track) {
    // file_.open("bin/switch.csv", std::ios::out | std::ios::app);
    file_.open("bin/" + FLAGS_race_id + "/switch-radius.csv");
    file_ << "previous_radius,start_radius,angle,end_radius,next_radius,piece_distance,radius" << std::endl;
  }

  void Record(const Position& position, double radius);

  // Returns 0 for straight line.
  double Radius(const Position& position);

 private:
  SwitchRadiusModel* GetModel(int piece, int start_lane) {
    if (models_[{piece, start_lane}] == nullptr) {
      models_[{piece, start_lane}].reset(new SwitchRadiusModel(piece, start_lane));
    }
    return models_[{piece, start_lane}].get();
  }

  // (piece, start_lane) -> model
  std::map<std::pair<int, int>, std::unique_ptr<SwitchRadiusModel>> models_;
  std::ofstream file_;

  const Track* track_;
};

}  // namespace game

#endif  // CPP_GAME_RADIUS_MODEL_H_
