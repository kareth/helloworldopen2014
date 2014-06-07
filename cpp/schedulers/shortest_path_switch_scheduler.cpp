#include "schedulers/shortest_path_switch_scheduler.h"

DEFINE_bool(overtake, true, "Should car overtake others?");
DECLARE_bool(log_overtaking);

using game::Position;
using game::Switch;

namespace schedulers {

ShortestPathSwitchScheduler::ShortestPathSwitchScheduler(
    const game::Race& race,
    game::RaceTracker& race_tracker,
    game::CarTracker& car_tracker)
  : race_(race), car_tracker_(car_tracker), race_tracker_(race_tracker),
    should_switch_now_(false), waiting_for_switch_(false),
    target_switch_(-1) {

  path_optimizer_.reset(new game::DoublePathOptimizer(race, car_tracker_));
}

bool ShortestPathSwitchScheduler::WaitingToReachIssuedSwitch(const game::CarState& state) {
  if (state.position().piece() == target_switch_) {
    waiting_for_switch_ = false;
    target_switch_ = -1;
  }
  return waiting_for_switch_;
}

// TODO IMPORTANT
// If we switch our decision last second, throttle scheduler may not
// be able to slow down to match switch curvature. We should check it!
//
// Updates and calculates next state
void ShortestPathSwitchScheduler::Schedule(const game::CarState& state) {
  if (WaitingToReachIssuedSwitch(state))
    return;

  // [0 - inf] = time loss for each decision compared to optimum
  auto time_loss = path_optimizer_->Score(state.position());

  // [-100 - 100] = (-)overtake, (0)neutral, (+) bump competitive
  auto obstacle_scores = race_tracker_.ScoreLanes(state);

  Switch best_direction = Switch::kStay;
  int best_score = -10000000;

  for (auto& el : time_loss) {
    Switch dir = el.first;

    // TODO improve weights on scores
    int score =
      - time_loss[dir]                       // ticks
      + std::min(0, obstacle_scores[dir]) * 1000  // overtaking strictly more important
      + std::max(0, obstacle_scores[dir]) * 1;    // bumping just a bit better

    if (score > best_score) {
      best_score = score;
      best_direction = dir;
    }
  }

  if (best_direction == Switch::kStay) {
    should_switch_now_ = false;
    target_switch_ = -1;
  } else {
    should_switch_now_ = true;
    target_switch_ = race_.track().NextSwitch(state.position().piece());
    direction_ = best_direction;
  }
}

void ShortestPathSwitchScheduler::Switched() {
  should_switch_now_ = false;
  waiting_for_switch_ = true;
}

bool ShortestPathSwitchScheduler::ShouldSwitch() {
  if (should_switch_now_ && !waiting_for_switch_) {
    auto s = car_tracker_.current_state();
    s = car_tracker_.Predict(s, game::Command(1));
    s = car_tracker_.Predict(s, game::Command(1));
    if (s.position().piece() != target_switch_)
      return false;

    if (car_tracker_.current_state().velocity() > 0 &&
        car_tracker_.IsSafe(car_tracker_.current_state(), game::Command(direction_)))
      return true;
  }
  return false;
}

game::Switch ShortestPathSwitchScheduler::ExpectedSwitch() {
  if (should_switch_now_ && !waiting_for_switch_) {
    return direction_;
  } else {
    return Switch::kStay;
  }
}

}  // namespace schedulers
