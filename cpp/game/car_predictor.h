#ifndef CPP_GAME_CAR_PREDICTOR_H_
#define CPP_GAME_CAR_PREDICTOR_H_

#include "game/command.h"
#include "game/position.h"

namespace game {

class CarState;

// The interface used during car simulation to predict the next state.
class CarPredictor {
 public:
  virtual CarState Predict(const CarState& state, const Command& command) = 0;
};

class CarState {
 public:
  // Creates the empty state (from start position).
  CarState() {}

  explicit CarState(const Position& position)
      : position_(position) {
  }
  CarState(const Position& position, double velocity, double previous_angle)
      : position_(position), velocity_(velocity), previous_angle_(previous_angle) {
  }

  const Position& position() const { return position_; }
  double velocity() const { return velocity_; }
  double previous_angle() const { return previous_angle_; }

 private:
  Position position_;
  double velocity_ = 0.0;
  double previous_angle_ = 0.0;
};

}  // namespace game

#endif  // CPP_GAME_CAR_PREDICTOR_H_
