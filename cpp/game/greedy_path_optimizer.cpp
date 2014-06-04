#include "game/greedy_path_optimizer.h"

namespace game {

GreedyPathOptimizer::GreedyPathOptimizer(const Race& race, CarTracker& car_tracker)
  : race_(race), car_tracker_(car_tracker) {
}

std::map<Switch, int> GreedyPathOptimizer::Score(const Position& position) {
  std::map<Switch, int> res;

  // We evaluate next two switches to decide if we should change the lane.
  int from = race_.track().NextSwitch(position.piece());
  int to = race_.track().NextSwitch(from);

  Position end_position;
  end_position.set_piece(to);

  int min_time = 1000000;
  for (int offset = -1; offset <= 1; offset++) {
    int lane = position.end_lane() + offset;
    if (race_.track().IsLaneCorrect(lane)) {
      double distance = DistanceBetween(position, end_position, lane);
      res[OffsetToDirection(offset)] = distance / kApproximateSpeed;
      min_time = min(min_time, res[OffsetToDirection(offset)]);
    }
  }

  // Convert time to time_loss
  for (auto el : res)
    res[el.first] = el.second - min_time;

  return res;
}

double GreedyPathOptimizer::DistanceBetween(const Position& position1, const Position& position2, int lane) {
  Position end_position = position2;
  end_position.set_start_lane(lane);
  end_position.set_end_lane(lane);

  return car_tracker_.DistanceBetween(position1, end_position);
}

Switch GreedyPathOptimizer::OffsetToDirection(int offset) {
  if (offset == -1) return Switch::kSwitchLeft;
  if (offset == 0) return Switch::kStay;
  if (offset == 1) return Switch::kSwitchRight;
  return Switch::kStay;
}

}  // namespace game
