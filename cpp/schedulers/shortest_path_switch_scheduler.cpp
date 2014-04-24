#include "schedulers/shortest_path_switch_scheduler.h"

// TODO refactor whole code here, its copypaste

using game::Position;

namespace schedulers {

ShortestPathSwitchScheduler::ShortestPathSwitchScheduler(
    const game::Race& race, game::CarTracker& car_tracker)
  : race_(race), car_tracker_(car_tracker), should_switch_now_(false),
    target_switch_(-1) {
  //ComputeShortestPaths();
}

// Prepares for overtake
void ShortestPathSwitchScheduler::Overtake(const string& color) {
  printf("Feature not implemented\n");
}

// Updates the state and calculates next state
// TODO switch time
void ShortestPathSwitchScheduler::Schedule(const game::CarState& state) {
  // TODO by far, it doesnt react on strategy change, it shouldnt be like that
  if (state.position().piece() == target_switch_)
    target_switch_ = -1;

  // already scheduled
  if (target_switch_ != -1 || should_switch_now_ == true)
    return;

  // TODO its greedy :D
  const Position& position = state.position();

  int from = NextSwitch(position.piece());
  int to = NextSwitch(from);

  double current = LaneLength(position, position.end_lane(), from, to);
  double left = 1000000000;
  double right = 1000000000;

  if (position.end_lane() > 0)
    left = LaneLength(position, position.end_lane() - 1, from, to);
  if (position.end_lane() < race_.track().lanes().size() - 1)
    right = LaneLength(position, position.end_lane() + 1, from, to);

  if (left < current && left < right) {
    scheduled_switch_ = game::Switch::kSwitchLeft;
    target_switch_ = from;
    should_switch_now_ = true;
  } else if (right < current && right <= left) {
    scheduled_switch_ = game::Switch::kSwitchRight;
    target_switch_ = from;
    should_switch_now_ = true;
  }
}

// From -> To excliding both
double ShortestPathSwitchScheduler::LaneLength(const game::Position& position, int lane, int from, int to) {
  double distance = 0;
  for (int p = from + 1; p < to; p++)
    distance += race_.track().LaneLength(p, lane);
  return distance;
}

// Next, not including given one
int ShortestPathSwitchScheduler::NextSwitch(int piece_index) {
  int index = 1;
  auto& pieces = race_.track().pieces();

  auto piece = pieces[(piece_index + 1) % pieces.size()];

  while (index <= pieces.size() && piece.has_switch() == false) {
    index++;
    piece = pieces[(piece_index + index) % pieces.size()];
  }
  return (piece_index + index) % pieces.size();
}

void ShortestPathSwitchScheduler::ComputeShortestPaths() {
  // TODO
}

void ShortestPathSwitchScheduler::Switched() {
  should_switch_now_ = false;
}

// Returns scheduled switch
bool ShortestPathSwitchScheduler::ShouldSwitch() {
  if (should_switch_now_) {
    if (car_tracker_.current_state().velocity() > 0 &&
        car_tracker_.IsSafe(
          car_tracker_.current_state(), game::Command(scheduled_switch_)))
      return true;
  }
  return false;
}

game::Switch ShortestPathSwitchScheduler::SwitchDirection() {
  return scheduled_switch_;
}

}  // namespace schedulers
