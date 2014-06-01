#ifndef CPP_GAME_VELOCITY_PREDICTOR_H_
#define CPP_GAME_VELOCITY_PREDICTOR_H_

#include "game/race.h"
#include "game/car_tracker.h"
#include <set>

namespace game {

struct state_compare {
    bool operator() (const CarState& lhs, const CarState& rhs) const{
      if (lhs.position().piece() < rhs.position().piece()) return true;
      if (lhs.position().piece() > rhs.position().piece()) return false;
      return lhs.position().piece_distance() < rhs.position().piece_distance();
    }
};

class VelocityPredictor {
 public:
  VelocityPredictor(CarTracker& car_tracker,
                    const Race& race);

  void Record(const CarState& state);
  double Velocity(const Position& position) const;

 private:
  void AddPoint(const CarState& state);
  double InterpolatePoint(const CarState& a, const CarState& b, const Position& p) const;
  bool HasDataToPredict(const Position& p) const;
  CarState Next(Position p) const;
  CarState Previous(Position p) const;
  Position PositionOnAnotherLane(const Position& p, int lane) const;
  void PrintData() const;

  static const int kMaxLanes = 10;

  std::set<CarState, state_compare> points[kMaxLanes];

  CarState state_;

  CarTracker& car_tracker_;
  const Race& race_;
};

}  // namespace game

#endif  // CPP_GAME_VELOCITY_PREDICTOR_H_
