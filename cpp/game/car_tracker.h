#ifndef CPP_GAME_CAR_TRACKER_H_
#define CPP_GAME_CAR_TRACKER_H_

#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <string>

#include "game/car_predictor.h"
#include "game/crash_model.h"
#include "game/drift_model.h"
#include "game/radius_model.h"
#include "game/error_tracker.h"
#include "game/lane_length_model.h"
#include "game/gauss.h"
#include "game/position.h"
#include "game/race.h"
#include "game/simplex.h"
#include "game/velocity_model.h"

#define SQR(X) ((X)*(X))

using std::map;

namespace game {

// Usage:
//
// CarTracker car_tracker(race);
//
// OnStep() {
//   car_tracker.Record(position);
//
//   // perform computation to choose best command
//   Command command = ...
//
//   car_tracker.RecordCommand(command);
// }
class CarTracker : public CarPredictor {
 public:
  CarTracker(const Race* race);

  virtual ~CarTracker();

  // We need to reset the CarTracker when the game is started.
  // The first Record() after Reset is assumed to be initial position of
  // the car.
  void Reset() {
    just_started_ = true;
  }

  void Record(const Position& position);

  void RecordCommand(const Command& command) {
    last_command_ = command;
  }

  void RecordCarCrash();

  void RecordTurboAvailable(const game::Turbo& turbo);

  CarState Predict(const CarState& state, const Command& command);

  bool IsSafe(const CarState& state, const Command& command);

  bool IsSafe(const CarState& state);


  bool IsReady() const;

  const CarState& current_state() {
    return state_;
  }

  // TODO deprecate
  double throttle() const { return last_command_.throttle(); }

  const CrashModel& crash_model() const { return crash_model_; }
  const VelocityModel& velocity_model() const { return velocity_model_; }

  // The following things are working differently:
  // - switch is always set to kStay
  // - turbo state is taken from previous tick
  CarState CreateCarState(const CarState& prev, const Position& position) const;

  void set_race(const Race* race) { race_ = race; }

 private:
  bool just_started_ = true;

  void LogState();
  double RadiusInPosition(const Position& position);

  std::ofstream stats_file_;

  CarState state_;
  Command last_command_;

  // Does not own the pointer.
  const Race* race_;

  CrashModel crash_model_;
  VelocityModel velocity_model_;
  DriftModel drift_model_;
  LaneLengthModel lane_length_model_;
  RadiusModel radius_model_;
};

}  // namespace game

#endif  // CPP_GAME_CAR_TRACKER_H_
