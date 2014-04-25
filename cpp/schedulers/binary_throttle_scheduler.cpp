#include "schedulers/binary_throttle_scheduler.h"

namespace schedulers {

using game::CarState;

BinaryThrottleScheduler::BinaryThrottleScheduler(const game::Race& race,
    game::CarTracker& car_tracker, int time_limit)
  : race_(race), car_tracker_(car_tracker), groups_() {
 // State with 16 groups, takes average 150ms, never exceeding 300ms
 // Each decrease reduces that time 2 times
 // Size = 16 seems quite optimal (1x1 + 15x2)

 int size = 16;

  for (; time_limit / 2 > 300 && size < 20; time_limit /= 2)
    size++;
  for (; time_limit < 300; time_limit *= 2)
    size--;

 // Default state consists of one 1 and multiple 2 later
 groups_.push_back(1);
 for (int i = 0; i < size - 1; i++)
   groups_.push_back(2);
}

BinaryThrottleScheduler::BinaryThrottleScheduler(const game::Race& race,
    game::CarTracker& car_tracker, const std::vector<int>& groups)
  : race_(race), car_tracker_(car_tracker), groups_(groups) {
}

// Returns scheduled throttle
double BinaryThrottleScheduler::throttle() {
  return schedule_[0];
}

void BinaryThrottleScheduler::Overtake(const string& color) {
  // TODO(kareth)
  printf("Feature not implemented.\n");
}

void BinaryThrottleScheduler::Schedule(const game::CarState& state) {
  // TODO(kareth) strategies
  if (strategy_ == Strategy::kOptimizeCurrentLap &&
      race_.track().IsLastStraight(state.position())) {
    schedule_ = { 1 };
  } else {
    Optimize(state);
    OptimizeTurboBrake(state);
  }
  Log(state);
}

void BinaryThrottleScheduler::OptimizeTurboBrake(const game::CarState& state) {
/*  // TODO hardcoded
  if (schedule_[0] < 1e-9) return;

  if (state.velocity() > 10) {
    // Move that will be issued now
    auto next = car_tracker_.Predict(state, game::Command(1));
    // if next one crashes, then we should start braking earlier
    next = car_tracker_.Predict(next, game::Command(1));
    // atleast 2 ticks :D
    next = car_tracker_.Predict(next, game::Command(1));
    if (!car_tracker_.IsSafe(next))
      schedule_[0] = 0;
  }*/
}

// Returns optimal throttlle:
void BinaryThrottleScheduler::Optimize(const CarState& state) {
  double distance;
  int mask = FindBestMask(state, &distance);
  if (mask == -1) mask = 0;

  schedule_.clear();
  for (int g = 0; g < groups_.size(); g++)
    for (int t = 0; t < groups_[g]; t++)
      schedule_.push_back((mask & (1 << g)) > 0);

  if (!car_tracker_.IsSafe(state))
    printf("Current state is not safe!\n");

  if (!car_tracker_.IsSafe(state, game::Command(schedule_[0])))
    schedule_[0] = 0;
}

// Finds most optimal(by means of distance travelled) mask
// @returns mask or -1 if impossible
// @param distance total distance travelled
int BinaryThrottleScheduler::FindBestMask(const CarState& state, double* distance) {
  int best_mask = -1;
  *distance = 0;

  // TODO dont predict that much. probably can go 2 times faster
  for (int mask = 0; mask < (1 << (groups_.size())); mask++) {
    double mask_distance;
    if (CheckMask(mask, state, &mask_distance) &&
        mask_distance > *distance) {
      *distance = mask_distance;
      best_mask = mask;
    }
  }
  return best_mask;
}

// Checks whether given throttle setup wont crash
// @returns false if car crashes
// @param distance total distance travelled
bool BinaryThrottleScheduler::CheckMask(
    int mask, const CarState& state, double* distance) {
  vector<CarState> states {state, state};
  int now = 1;
  *distance = 0;

  for (int g = 0; g < groups_.size(); g++) {
    for (int t = 0; t < groups_[g]; t++) {
      states[now ^ 1] = car_tracker_.Predict(states[now], game::Command((mask & (1 << g)) > 0));
      now ^= 1;
      (*distance) += race_.track().Distance(states[now].position(), states[now ^ 1].position());
      if (!car_tracker_.crash_model().IsSafe(states[now].position().angle()))
        return false;
    }
  }
  return true;
}

void BinaryThrottleScheduler::Log(const game::CarState& state) {
  int i = 0;
  for (int g = 0; g < groups_.size(); g++) {
    if (g >= schedule_.size()) break;
    std::cout << (int) schedule_[i] << " ";
    for (int t = 0; t < groups_[g]; t++)
      i++;
  }
  std::cout << "(" << state.position().piece() << ")" << std::endl;
}

}  // namespace schedulers
