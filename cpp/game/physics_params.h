#ifndef CPP_GAME_PHYSICS_PARAMS_H
#define CPP_GAME_PHYSICS_PARAMS_H

#include <vector>
#include <cmath>

namespace game {

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

// TODO
class SwitchLengthParams {
};

// TODO
class SwitchRadiusParams {

};

class PhysicsParams {
 public:
  VelocityModelParams velocity_model_params;
  DriftModelParams drift_model_params;

  static PhysicsParams Load() {
    return PhysicsParams();
  }
};

}  // namespace game

#endif  // CPP_GAME_PHYSICS_PARAMS_H
