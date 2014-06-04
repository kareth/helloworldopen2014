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

  path_optimizer_.reset(new game::GreedyPathOptimizer(race, car_tracker_));
}

// Updates and calculates next state
void ShortestPathSwitchScheduler::Schedule(const game::CarState& state) {
  // We missed recently scheduled switch
  if (state.position().piece() == target_switch_) {
    waiting_for_switch_ = false;
    target_switch_ = -1;
  }

  // If we already issued command
  if (waiting_for_switch_)
    return;

  auto time_scores = path_optimizer_->Score(state.position());
  /*auto obstacle_scores = race_tracker_.ScoreLanes(state);

  Switch best_decision = Switch::kStay;
  int best_score = 0;

  for (auto& el : time_scores) {
    auto decision = el.first;

  }*/

  double left = kInf, current = kInf, right = kInf;
  if (time_scores.find(Switch::kSwitchLeft) != time_scores.end())
    left = time_scores[Switch::kSwitchLeft];
  if (time_scores.find(Switch::kStay) != time_scores.end())
    current = time_scores[Switch::kStay];
  if (time_scores.find(Switch::kSwitchRight) != time_scores.end())
    right = time_scores[Switch::kSwitchRight];

  const Position& position = state.position();
  int from = race_.track().NextSwitch(position.piece());
  int to = race_.track().NextSwitch(from);

  // Overtaking
  if (FLAGS_overtake) {
    int left_score = race_tracker_.ScoreLane(from, to, position.end_lane() - 1);
    int right_score = race_tracker_.ScoreLane(from, to, position.end_lane() + 1);
    int current_score = race_tracker_.ScoreLane(from, to, position.end_lane());

    if (left_score < 0) left = kInf;
    if (right_score < 0) right = kInf;
    if (current_score < 0) current = kInf;

    // Score is <-10, 0), 0 is best
    // Nest part of algorithm chooses smallest so we need to reverse it temporarily
    if (left_score < 0 && current_score < 0 && right_score < 0) {
      left = -left_score;
      current = -current_score;
      right = -current_score;
    }
  }

  if (FLAGS_log_overtaking)
    printf("Lane scores (l c r): %lf %lf %lf\n",left, current, right);

  if (left < current && left < right) {
    direction_ = Switch::kSwitchLeft;
    target_switch_ = from;
    should_switch_now_ = true;
  } else if (right < current && right <= left) {
    direction_ = Switch::kSwitchRight;
    target_switch_ = from;
    should_switch_now_ = true;
  } else {
    should_switch_now_ = false;
    target_switch_ = -1;
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
