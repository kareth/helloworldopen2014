#include "schedulers/switch_scheduler.h"

namespace bots {
namespace stepping {

  /*
// TODO refactor
bool ShouldChangeLane(const game::CarState& state, game::Switch* s) {
  const Position& position = state.position();
  // TODO its just just basic greedy choosing
  if (position.piece() == switched_)
    switched_ = -1;

  if (switched_ != -1)
    return false;

  int from = NextSwitch(position.piece());

  if (car_tracker_->Predict(state, Command(1)).position().piece() != from)
    return false;

  int to = NextSwitch(from);

  double current = LaneLength(position, position.end_lane(), from, to);
  double left = 1000000000;
  double right = 1000000000;

  if (position.start_lane() > 0)
    left = LaneLength(position, position.end_lane() - 1, from, to);
  if (position.end_lane() < race_.track().lanes().size() - 1)
    right = LaneLength(position, position.end_lane() + 1, from, to);

  if (left < current && left < right) {
    *s = game::Switch::kSwitchLeft;
    switched_ = from;
    return true;
  }
  else if (right < current && right <= left) {
    *s = game::Switch::kSwitchRight;
    switched_ = from;
    return true;
  }
  return false;
}

// From -> To excliding both
double LaneLength(const game::Position& position, int lane, int from, int to) {
  double distance = 0;
  for (int p = from + 1; p < to; p++)
    distance += race_.track().LaneLength(p, lane);
  return distance;
}

// Next, not including given one
int NextSwitch(int piece_index) {
  int index = 1;
  auto& pieces = race_.track().pieces();

  auto piece = pieces[(piece_index + 1) % pieces.size()];

  while (index <= pieces.size() && piece.has_switch() == false) {
    index++;
    piece = pieces[(piece_index + index) % pieces.size()];
  }
  return (piece_index + index) % pieces.size();
}
*/
}  // namespace stepping
}  // namespace bots
