#ifndef CPP_GAME_ENEMY_TRACKER_H_
#define CPP_GAME_ENEMY_TRACKER_H_

#include <string>
#include <map>
#include "game/race.h"
#include "game/car_tracker.h"
#include "game/velocity_predictor.h"

namespace game {

class EnemyTracker {
 public:
  EnemyTracker(game::CarTracker& car_tracker, const game::Race& race,
      const std::string& color, const Position& position);

  // Record methods
  // lap time is measured in game ticks
  void RecordLapTime(int time);
  void RecordPosition(const game::Position& position);
  void RecordCrash();
  void DNF();
  void FinishedRace();
  void Resurrect();
  void Spawned();
  void TurboStarted();
  void NewTurbo(const Turbo& turbo);

  // Returns position after @time ticks
  // target lane sets switch state of the car to that lane
  Position PositionAfterTime(int time, int target_lane = -1) const;

  // Returns number of ticks to pass the position p
  int TimeToPosition(const Position& p) const;

  // Returns predicted velocity at position p.
  // Doesnt count the situation where we are dead
  int ExpectedVelocity(const Position& p) const { return Velocity(p); }

  // OBSOLETE
  // Approximation-wise
  bool CanOvertake(const EnemyTracker& noobek, int from, int to);

  bool is_dead() const { return dead_; }
  bool is_accelerating() const { return accelerating_; }

  // True means that we wont ever have to care about him
  bool has_finished() const { return disabled_ || finished_; }

  // TODO is it necessary with new predictor?
  // Means that we have enough data to predict velocity
  bool IsReady() const { return ready_; }

  // Returns number of ticks to car spawn
  // 1 means that the car will be back on track in the
  // moment of next CarPositions
  // 0 means that its on track now
  int time_to_spawn() const {
    return std::max(0, car_tracker_.spawn_model().duration() - time_since_crash_ + 1); }

  // Returns best lap time in ticks
  int best_lap() const { return best_lap_; }

  // Current state
  const CarState& state() const { return state_; }

  // Car color
  const std::string& color() const { return color_; }

 private:
  bool ShouldRecord() const;
  double Velocity(const Position& position) const;
  void RecordTick(const game::Position& position);

  std::vector<int> lap_times_;
  int best_lap_ = -1;

  std::vector<double> piece_speed_;
  std::vector<int> piece_data_points_;

  double average_speed_;
  int average_data_points_;

  // Enemy state and color
  CarState state_;
  std::string color_;

  const Race& race_;
  CarTracker& car_tracker_;

  // Car is just accelerating. Skip some slow ticks
  static const int kSkipTime = 20;
  int skip_time_ = kSkipTime;

  // Car has crashed or started recently. Skip some slow ticks
  bool accelerating_ = true;

  // Car crashed, wait until it spawns
  bool dead_ = false;

  // Car finished race. Can return in next race
  bool finished_ = false;

  // Car was disqualified, wont ever return
  bool disabled_ = false;

  int time_since_crash_ = 0;

  bool ready_ = false;

  VelocityPredictor velocity_predictor_;
};
}  // namespace game

#endif  // CPP_GAME_ENEMY_TRACKER_H_
