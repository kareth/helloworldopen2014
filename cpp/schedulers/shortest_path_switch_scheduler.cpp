#include "schedulers/shortest_path_switch_scheduler.h"

DEFINE_bool(overtake, true, "Should car overtake others?");
DECLARE_bool(log_overtaking);

DEFINE_bool(safe_switches, false, "Switch first 2 switches");

using game::Position;
using game::Switch;

namespace schedulers {

ShortestPathSwitchScheduler::ShortestPathSwitchScheduler(
    const game::Race& race,
    game::RaceTracker& race_tracker,
    game::CarTracker& car_tracker)
  : race_(race), car_tracker_(car_tracker), race_tracker_(race_tracker),
    should_switch_now_(false), waiting_for_switch_(false),
    target_switch_(-1), target_lane_(-1), last_throttle_(0), already_issued_switch_(Switch::kStay) {
  path_optimizer_.reset(new game::DoublePathOptimizer(race, car_tracker_));
  throttle_scheduler_.reset(WojtekThrottleScheduler::CreateQuickScheduler(race, car_tracker));
}

bool ShortestPathSwitchScheduler::WaitingToReachIssuedSwitch(const game::CarState& state) {
  // TODO doesnt work in special case:
  // If we fail to switch scheduled switch AND in the same time we didnt get any tick
  // on switch piece :D
  if (state.position().end_lane() == target_lane_ ||
      target_switch_ == state.position().piece()) {
    should_switch_now_ = false;
    waiting_for_switch_ = false;
    already_issued_switch_ = Switch::kStay;
    target_switch_ = -1;
    target_lane_ = state.position().end_lane();
    direction_ = Switch::kStay;
  }
  return waiting_for_switch_;
}

void ShortestPathSwitchScheduler::Schedule(const game::CarState& state, const utils::Deadline& deadline) {
  state_ = state;

  // Check if we reached target switch
  WaitingToReachIssuedSwitch(state);

  // [0 - inf] = time loss for each decision compared to optimum
  auto time_loss = path_optimizer_->Score(state.position());
  // [-100 - 100] = (-)overtake, (0)neutral, (+) bump competitive
  auto obstacle_scores = race_tracker_.ScoreLanes(state, deadline);
  vector<std::pair<double, Switch>> scores;

  for (auto& el : time_loss) {
    Switch dir = el.first;
    double score =
      - time_loss[dir]                       // ticks
      + std::min(0, obstacle_scores[dir]) * 1000.0  // overtaking strictly more important
      + std::max(0, obstacle_scores[dir]) * 1.0;    // bumping just a bit better
    scores.push_back({ score, dir });
  }
  sort(scores.begin(), scores.end(), [] (const std::pair<double, Switch>& a, const std::pair<double, Switch>& b) {
        if (a.first > b.first) return true;
        if (a.first < b.first) return false;
        if (a.second == Switch::kStay) return true;
        return a.second > b.second;
      });  // Last are the best


  if (FLAGS_safe_switches) {
    int first_switch = race_.track().NextSwitch(0);
    int second_switch = race_.track().NextSwitch(first_switch);
    int target_switch = race_.track().NextSwitch(state.position().piece());

    if (race_.race_phase() &&
        state.position().lap() == 0 &&
        (target_switch == first_switch || target_switch == second_switch)) {
      scores.push_back(scores[0]);
      scores.erase(scores.begin());
    }
  }


  if (FLAGS_log_overtaking) {
    printf("Total Scores: \n");
    for (auto p : scores) {
      printf("{%lf %d}  ",p.first, p.second);
    }
    printf("\n");
  }


  auto target = Position(race_.track().NextSwitch(state.position().piece()), 0);
  target.set_start_lane(state_.position().end_lane());
  target.set_end_lane(state_.position().end_lane());
  double distance = car_tracker_.DistanceBetween(state_.position(), target);  // Distance to switch
  if (FLAGS_log_overtaking)
    printf("distance: %lf\n",distance);

  for (int i = 0; i < scores.size(); i++) {
    auto dir = scores[i].second;
    if (!IsChangeDecision(dir)) {
      if (FLAGS_log_overtaking) printf("no_change\n");
      return;  // the same as before

    } else {
      if (FLAGS_log_overtaking) printf("change %d\n", dir);

      if (dir == Switch::kStay && already_issued_switch_ != Switch::kStay)
        continue;  // We cannot stop switch command...

      auto state_with_switch = state;
      state_with_switch.set_switch_state(dir);
      if (throttle_scheduler_->Schedule(state_, -1, utils::Deadline(std::chrono::microseconds(200)),
            distance, last_throttle_)) {
        if (FLAGS_log_overtaking) printf("Safe!\n");

        if (dir == Switch::kStay) {
          waiting_for_switch_ = false;
          should_switch_now_ = false;
          target_switch_ = -1;
          target_lane_ = state.position().end_lane();
        } else {
          if (dir != already_issued_switch_) { // If we already issued that switch, we dont have to care
            should_switch_now_ = true;
            waiting_for_switch_ = false;
          }
          target_switch_ = race_.track().NextSwitch(state.position().piece());
          target_lane_ = state.position().end_lane() + int(dir) - 1;
          direction_ = dir;
        }
        return;  // found best
      }
    }
  }
}

bool ShortestPathSwitchScheduler::IsChangeDecision(const Switch& s) {
  return target_lane_ != state_.position().end_lane() + int(s) - 1;
}

double ShortestPathSwitchScheduler::DistanceToSwitch() {
  if (FLAGS_log_overtaking) printf(" [%d %d %d / %d] \n", waiting_for_switch_, should_switch_now_, target_switch_, already_issued_switch_);
  if (waiting_for_switch_)
    return -1;
  if (should_switch_now_) {
    Position target(target_switch_, 0);
    target.set_start_lane(state_.position().end_lane());
    target.set_end_lane(state_.position().end_lane());
    //std::cout << "s: " << state_.position().ShortDebugString() << std::endl;
    //std::cout << "t: " << target.ShortDebugString() << std::endl;
    return car_tracker_.DistanceBetween(state_.position(), target);
  }
  return -1;
}

void ShortestPathSwitchScheduler::Switched() {
  should_switch_now_ = false;
  waiting_for_switch_ = true;
  already_issued_switch_ = direction_;
}

game::Switch ShortestPathSwitchScheduler::ExpectedSwitch() {
  if (should_switch_now_ && !waiting_for_switch_) {
    return direction_;
  } else {
    return Switch::kStay;
  }
}


// OBSOLETE!!!!!!!!
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

}  // namespace schedulers
