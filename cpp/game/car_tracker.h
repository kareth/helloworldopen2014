#ifndef CPP_GAME_CAR_TRACKER_H_
#define CPP_GAME_CAR_TRACKER_H_

#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <string>
#include <cmath>

#include "game/physics_params.h"
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
#include "game/spawn_model.h"
#include "game/physics_params.h"

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
  CarTracker(const Race* race, const PhysicsParams& params);

  virtual ~CarTracker();

  // We need to reset the CarTracker when the game is started.
  // The first Record() after Reset is assumed to be initial position of
  // the car.
  void Reset() {
    just_started_ = true;
    last_record_had_bump = false;
  }

  void Record(const Position& position, bool bump=false);

  void RecordCommand(const Command& command) {
    last_command_ = command;
  }

  void RecordCarCrash();

  void RecordTurboAvailable(const game::Turbo& turbo);

  CarState Predict(const CarState& state, const Command& command);

  double PredictDistance(const CarState& state, const vector<double>& throttles) const;
  double PredictDistance(const CarState& state, int how_many, double throttle) const;

  bool IsSafe(const CarState& state, const Command& command);

  bool IsSafe(const CarState& state, double safe_speed = 2);
  // TODO hardcoded safe_speed

  // TODO Add nice comments
  // Slower version of IsSafe() but more correct
  // - if velocity = 0 but angular_velocity > 0, we will report not safe
  // - it will try to first accellarate if Command(0) failed
  bool GenerateSafeStates(const CarState& state, vector<CarState>* states);

  bool IsReady() const;

  const CarState& current_state() {
    return state_;
  }

  // TODO deprecate
  double throttle() const { return last_command_.throttle(); }

  const CrashModel& crash_model() const { return crash_model_; }
  const VelocityModel& velocity_model() const { return velocity_model_; }
  SpawnModel& spawn_model() { return spawn_model_; }

  // The following things are working differently:
  // - switch is always set to kStay
  // - turbo state is taken from previous tick
  CarState CreateCarState(const CarState& prev, const Position& position) const;

  void set_race(const Race* race) { race_ = race; }

  // Start from 'position1', how much do I have to go to get to 'position2'.
  //
  // If positions are on different lanes, we assume we would switch lanes
  // at first available position.
  //
  // 'is_perfect' is output parameter used to determine if we are sure that the
  // distance is correct. If false it means that the distance can be slightly
  // off. The reason for incorrect distance are switches (we didn't come up with
  // the perfect model for them so we need to drive through them first).
  //
  // 'max_distance' is optional parameter used to limit number of operations
  // and stop computing the distance if it is higher. It does NOT mean we will
  // always return smaller value.
  //
  // WARNING: We only iterate over const number of  pieces to compute the
  // distance, so if we need to travel more to get to position2, the returned
  // distance can be too small. But because we use it mainly to compute small
  // distances (someone just ahead of use or someone just behind us), it
  // shouldn't matter.
  double DistanceBetween(const Position& position1, const Position& position2,
                         bool* is_perfect=nullptr, double max_distance=10000000) const;


  // Returns true if the car starting from "car_state" can reach "target" using
  // "ticks" ticks.
  //
  // If true is returned, min_velocity will contain the minimal velocity the car
  // will have in "target" position.
  //
  // Assumptions / BUGS:
  // - car_state.position and target has to be correct for DistanceBetween()
  //   - no switches, etc.
  // - no turbo :(
  // - it is only lower bound (it is possible such min_velocity cannot be achieved.
  bool MinVelocity(const CarState& car_state, int ticks, const Position& target, double* min_velocity, int* full_throttle_ticks) const;

  // Returns position that is "distance" units farther. In case we encounter
  // switches, we assume we are trying to reach target_lane.
  //
  // If target_lane is -1, then we assume target_lane=position.end_lane().
  //
  // We assume distance > 0.
  //
  // WARNING: We only iterate over const number of  pieces to compute the
  // position, so if we need to travel more to get to position2, the returned
  // position can be incorrect. But because we use it mainly to compute small
  // distances (someone just ahead of use or someone just behind us), it
  // shouldn't matter.
  Position PredictPosition(const Position& position, double distance, int target_lane=-1) const;

  // Returns true, if there is high probability that there was a bump.
  bool HasSomeoneMaybeBumpedMe(const map<string, Position>& positions, const std::string& color);

  // Returns false if there is no way to keep the radius.
  // If true, then throttle contains maximum throttle that will keep
  // car on piece with the same radius.
  bool BoundaryThrottle(const CarState& car_state, double* throttle);

  struct Curve {
    int direction = 0;
    // For straight pieces, it will be 0.
    double radius = 0;
    double distance = 0.0; // Distance from a given car state to begin of this curve
    double length = 0.0;   // Distance from the begin (or car position) to the end of the curve

    // Square root ratius and 0 is converted to an arbitrary non-zero number
    double rradius() const {
        if (radius == 0.0) // Straight line
            return 10;  //TODO(Wojtek): Could be better for smoothness (e.g. last radius)
        return pow(radius, 0.5);
    }

    Curve() {}
    Curve(int direction, double radius, double distance) :
      direction(direction), radius(radius), distance(distance) {}
  };

  // Returns empty vector if car is on the switch.
  vector<Curve> GetCurves(const CarState& car_state, double distance);

  const CrashModel& GetCrashModel() const { return crash_model_; }

  const VelocityModel& GetVelocityModel() const { return velocity_model_; }

  const DriftModel& GetDriftModel() const { return drift_model_; }

  PhysicsParams CreatePhysicsParams();

  const LaneLengthModel& lane_length_model() { return lane_length_model_; }


  // Returns true if 'enemy_state' can be in such position after 'ticks_after'
  // that 'my_state' will hit 'enemy_state'. If true, also returns the min
  // velocity of the enemy.
  //
  // 'my_state' - state after 'ticks_after' ticks
  // 'enemy_state' - state that we should simulate 'ticks_after' ticks from.
  // 'ticks_after' - ticks to simulate
  // 'min_velocity' - only set if method returns true
  //
  // This method is safe, but can sometimes return true (there was a bump)
  // even if bump was not possible (because of switches).
  //
  // Note: This method can be slow O(ticks^2) but performance can be easily
  // improved if needed.
  bool CanBumpAfterNTicks(
      const CarState& my_state,
      const CarState& enemy_state,
      int ticks_after,
      double* min_velocity) const;

  bool IsBumpInevitable(
      const CarState& my_state_before,
      const CarState& my_state_after,
      const CarState& enemy_state,
      int ticks_after) const;


  bool IsSafeAttack(
    const CarState& current_state,
    const CarState& enemy_state,
    Command* command,
    bool allow_turbo=false);

  // FOR UNITTESTS ONLY
  CrashModel* mutable_crash_model() { return &crash_model_; }

 private:
  bool HasBumped(
      const CarState& state1,
      const CarState& state2);

  bool IsSafeAttackWithoutSwitches(
    const CarState& current_state,
    const CarState& enemy_state,
    Command* command,
    bool allow_turbo);

  void LogState();
  double RadiusInPosition(const Position& position);
  bool InternalGenerateSafeStates(const CarState& state, vector<CarState>* states);
  bool InternalAddStates(const CarState& state, Command command, int count, vector<CarState>* states);

  const int kDistanceBetweenIter = 500;
  bool just_started_ = true;
  bool last_record_had_bump = false;

  CarState state_;
  Command last_command_;

  // Does not own the pointer.
  const Race* race_;

  CrashModel crash_model_;
  VelocityModel velocity_model_;
  DriftModel drift_model_;
  LaneLengthModel lane_length_model_;
  RadiusModel radius_model_;
  SpawnModel spawn_model_;
};

}  // namespace game

#endif  // CPP_GAME_CAR_TRACKER_H_
