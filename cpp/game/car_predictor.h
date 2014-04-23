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
  CarState(const Position& position, double velocity, double previous_angle,
           Switch switch_state, double throttle)
      : position_(position), velocity_(velocity), previous_angle_(previous_angle),
        switch_(switch_state), throttle_(throttle) {
  }

  const Position& position() const { return position_; }
  double velocity() const { return velocity_; }

  // We need this to compute angular velocity
  double previous_angle() const { return previous_angle_; }

  // Are we planing to switch the lane?
  Switch switch_state() const { return switch_; }

  // If someone performs other command than throttle, we need to use throttle
  // from last tick, so we need to store it in the car state.
  double throttle() const { return throttle_; }

 private:
  Position position_;
  Switch switch_ = Switch::kStay;
  double throttle_ = 0.0;
  double velocity_ = 0.0;
  double previous_angle_ = 0.0;
};

}  // namespace game

#endif  // CPP_GAME_CAR_PREDICTOR_H_
