#include "schedulers/shortest_path_switch_scheduler.h"

DEFINE_bool(overtake, true, "Should car overtake others?");

// TODO refactor whole code here, its copypaste

using game::Position;

namespace schedulers {

ShortestPathSwitchScheduler::ShortestPathSwitchScheduler(
    const game::Race& race,
    game::RaceTracker& race_tracker,
    game::CarTracker& car_tracker)
  : race_(race), car_tracker_(car_tracker), race_tracker_(race_tracker),
    should_switch_now_(false), waiting_for_switch_(false),
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
  if (state.position().piece() == target_switch_) {
    waiting_for_switch_ = false;
    target_switch_ = -1;
  }
  if (waiting_for_switch_)
    return;

  // TODO its greedy :D
  const Position& position = state.position();

  int from = race_.track().NextSwitch(position.piece());
  int to = race_.track().NextSwitch(from);

  // TODO lane model?
  double current = LaneLength(position, position.end_lane(), from, to);
  double left = kInf;
  double right = kInf;

  if (position.end_lane() > 0)
    left = LaneLength(position, position.end_lane() - 1, from, to);
  if (position.end_lane() < race_.track().lanes().size() - 1)
    right = LaneLength(position, position.end_lane() + 1, from, to);

  // Overtaking
  if (FLAGS_overtake) {
    if (!IsLaneSafe(state, from, to - 1, position.end_lane() - 1)) left = kInf;
    if (!IsLaneSafe(state, from, to - 1, position.end_lane() + 1)) right = kInf;
    if (!IsLaneSafe(state, from, to - 1, position.end_lane()))     current = kInf;
  }

  if (left < current && left < right) {
    scheduled_switch_ = game::Switch::kSwitchLeft;
    target_switch_ = from;
    should_switch_now_ = true;
  } else if (right < current && right <= left) {
    scheduled_switch_ = game::Switch::kSwitchRight;
    target_switch_ = from;
    should_switch_now_ = true;
  } else {
    should_switch_now_ = false;
    target_switch_ = -1;
  }
}

bool ShortestPathSwitchScheduler::IsLaneSafe(const game::CarState& state,
    int from, int to, int lane) {
  auto cars = race_tracker_.PredictedCarsBetween(from, to, lane);

  if (cars.size() > 0) {
    //printf("Unsafe lane! %d\n", lane);
    //for (auto c : cars)
    //  std::cout << c << " ";
    //std::cout << std::endl;
    return false;
  }
  return true;
}

// From -> To excliding both
double ShortestPathSwitchScheduler::LaneLength(const game::Position& position, int lane, int from, int to) {
  double distance = 0;
  for (int p = from + 1; p < to; p++)
    distance += race_.track().LaneLength(p, lane);
  return distance;
}

void ShortestPathSwitchScheduler::ComputeShortestPaths() {
  // TODO
}

void ShortestPathSwitchScheduler::Switched() {
  should_switch_now_ = false;
  waiting_for_switch_ = true;
}

// Returns scheduled switch
bool ShortestPathSwitchScheduler::ShouldSwitch() {
  if (should_switch_now_ &&
      !waiting_for_switch_) {
    auto s = car_tracker_.current_state();
    s = car_tracker_.Predict(s, game::Command(1));
    s = car_tracker_.Predict(s, game::Command(1));
    if (s.position().piece() != target_switch_)
      return false;

    if (car_tracker_.current_state().velocity() > 0 &&
        car_tracker_.IsSafe(car_tracker_.current_state(), game::Command(scheduled_switch_)))
      return true;
  }
  return false;
}

game::Switch ShortestPathSwitchScheduler::SwitchDirection() {
  return scheduled_switch_;
}

}  // namespace schedulers
