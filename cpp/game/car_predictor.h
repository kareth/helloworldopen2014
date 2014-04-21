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
  CarState() {}
  explicit CarState(const Position& position)
      : position_(position) {
  }
  CarState(const Position& position, double velocity, double angular_velocity)
      : position_(position), velocity_(velocity), angular_velocity_(angular_velocity) {
  }

  const Position& position() const { return position_; }
  const double velocity() const { return velocity_; }
  const double angular_velocity() const { return angular_velocity_; }

 private:
  Position position_;
  double velocity_ = 0.0;
  double angular_velocity_ = 0.0;
};

}  // namespace game

#endif  // CPP_GAME_CAR_PREDICTOR_H_
