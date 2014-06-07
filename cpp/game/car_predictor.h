#ifndef CPP_GAME_CAR_PREDICTOR_H_
#define CPP_GAME_CAR_PREDICTOR_H_

#include "game/command.h"
#include "game/position.h"
#include "game/turbo.h"

namespace game {

class CarState;

// The interface used during car simulation to predict the next state.
class CarPredictor {
 public:
  virtual CarState Predict(const CarState& state, const Command& command) = 0;
};

class TurboState {
 public:
  TurboState() {}

  bool available() const { return available_ && !is_on_; }
  const Turbo& turbo() const { return turbo_; }
  bool is_on() const { return is_on_; }
  int ticks_left() const { return ticks_left_; }

  void Enable() {
    if (!available_) std::cerr << "Cannot enable turbo if unavailable" << std::endl;
    if (is_on_) {
      std::cerr << "Cannot enable turbo if already on!" << std::endl;
    } else {
      available_ = false;
      is_on_ = true;
      turbo_in_use_ = turbo_;
      ticks_left_ = turbo_.duration() + 2;
    }
  }

  void Decrement() {
    if (is_on_) {
      ticks_left_--;
      if (ticks_left_ == 0) {
        is_on_ = false;
      }
    }
  }

  void Add(const Turbo& turbo) {
    turbo_ = turbo;
    available_ = true;
  }

  void Reset() {
    available_ = false;
    is_on_ = false;
    ticks_left_ = 0;
  }

 private:
  bool is_on_ = false;
  Turbo turbo_in_use_;
  int ticks_left_ = 0;

  bool available_ = false;
  Turbo turbo_;
};

class CarState {
 public:
  // Creates the empty state (from start position).
  CarState() {}

  explicit CarState(const Position& position)
      : position_(position) {
  }
  CarState(const Position& position, double velocity, double previous_angle,
           Switch switch_state, double throttle, TurboState turbo_state)
      : position_(position), velocity_(velocity), previous_angle_(previous_angle),
        switch_(switch_state), throttle_(throttle), turbo_state_(turbo_state) {
  }

  const Position& position() const { return position_; }
  double velocity() const { return velocity_; }

  // We need this to compute angular velocity
  double previous_angle() const { return previous_angle_; }

  // Are we planing to switch the lane?
  Switch switch_state() const { return switch_; }
  void set_switch_state(Switch s) { switch_ = s; }

  // If someone performs other command than throttle, we need to use throttle
  // from last tick, so we need to store it in the car state.
  double throttle() const { return throttle_; }

  TurboState turbo_state() const { return turbo_state_; }

  void AddNewTurbo(const Turbo& turbo) {
    turbo_state_.Add(turbo);
  }

  void EnableTurbo() { turbo_state_.Enable(); }

  void ResetTurbo() { turbo_state_.Reset(); }

  std::string DebugString() const {
    std::stringstream ss;
    ss << "position: " << position_.DebugString();
    ss << "switch: " << as_integer(switch_) << std::endl;
    ss << "velocity: " << velocity_ << std::endl;
    ss << "previous_angle: " << previous_angle_ << std::endl;
    return ss.str();
  }

  std::string ShortDebugString() const {
    std::stringstream ss;
    ss << "position: " << position_.ShortDebugString();
    ss << " switch: " << as_integer(switch_);
    ss << " velocity: " << velocity_;
    ss << " previous_angle: " << previous_angle_;
    return ss.str();
  }

  // This is used to predict bumps. Maybe we should write
  // method in car tracker (Bump(state1, state2)?).
  void set_velocity(double velocity) {
    velocity_ = velocity;
  }

 private:
  Position position_;
  Switch switch_ = Switch::kStay;
  TurboState turbo_state_;
  double throttle_ = 0.0;
  double velocity_ = 0.0;
  double previous_angle_ = 0.0;
};

}  // namespace game

#endif  // CPP_GAME_CAR_PREDICTOR_H_
