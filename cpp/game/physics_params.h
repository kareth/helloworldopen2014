#ifndef CPP_GAME_PHYSICS_PARAMS_H
#define CPP_GAME_PHYSICS_PARAMS_H

#include <vector>
#include <cmath>
#include <map>

namespace game {

class Track;

class VelocityModelParams {
 public:
  // Model used to drive.
  // next_velocity = x0 * velocity + x1 * throttle
  std::vector<double> model;

  VelocityModelParams() : model({0.98, 0.2}) {}
};

class DriftModelParams {
 public:
  // next_angle = x0 * angle +
  //              x1 * previous_angle +
  //              x2 * angle * velocity +
  //              -direction * max(0, x3 * velocity * velocity * sqrt(1 / radius) -
  //                                  x4 * velocity)
 std::vector<double> model;

 DriftModelParams() : model({1.9, -0.9, -0.00125, 0.00125 * sqrt(180000), 0.3}) {}
};

class SwitchLengthParams {
 public:
  // {length, width} => switch_length
  std::map<std::pair<double, double>, double> switch_on_straight_length;
  // {start_radius, end_radius, angle} => switch_length
  std::map<std::tuple<double, double, double>, double> switch_on_turn_length;

  // Loads the params from file.
  void Load();
  void Save() const;

  // Logs to stdout the switches from track that are unknown.
  void LogMissingData(const Track& track) const;
};

// TODO
class SwitchRadiusParams {

};

class PhysicsParams {
 public:
  VelocityModelParams velocity_model_params;
  DriftModelParams drift_model_params;
  SwitchLengthParams switch_length_params;

  static PhysicsParams Load() {
    PhysicsParams params;
    params.switch_length_params.Load();
    return params;
  }

  void Save() const {
    switch_length_params.Save();
  }
};

}  // namespace game

#endif  // CPP_GAME_PHYSICS_PARAMS_H
