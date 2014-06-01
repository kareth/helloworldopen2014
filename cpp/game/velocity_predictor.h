#ifndef CPP_GAME_VELOCITY_PREDICTOR_H_
#define CPP_GAME_VELOCITY_PREDICTOR_H_

#include "game/race.h"
#include "game/car_tracker.h"
#include <set>

namespace game {

struct state_compare {
    bool operator() (const CarState& lhs, const CarState& rhs) const{
        return lhs.velocity() < rhs.velocity();
    }
};

class VelocityPredictor {
 public:
  VelocityPredictor(CarTracker& car_tracker,
                    const Race& race);

  void Record(const CarState& state);
  double Velocity(const Position& position);

 private:
  void AddPoint(const CarState& state);
  double InterpolatePoint(const CarState& a, const CarState& b, const Position& p);
  bool HasDataToPredict(const Position& p);
  CarState Next(Position p);
  CarState Previous(Position p);
  Position PositionOnAnotherLane(const Position& p, int lane);

  static const int kMaxLanes = 10;

  std::set<CarState, state_compare> points[kMaxLanes];

  CarState state_;

  CarTracker& car_tracker_;
  const Race& race_;
};

}  // namespace game

#endif  // CPP_GAME_VELOCITY_PREDICTOR_H_
