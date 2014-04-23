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
 for (int i = 0; i < size - 2; i++)
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
  Optimize(state);
}

// Returns optimal throttlle:
void BinaryThrottleScheduler::Optimize(const CarState& state) {
  double distance;
  int mask = FindBestMask(state, &distance);

  schedule_.clear();
  for (int g = 0; g < groups_.size(); g++)
    for (int t = 0; t < groups_[g]; t++)
      schedule_.push_back((mask & (1 << g)) > 0);

  Log(state);
}

// Finds most optimal(by means of distance travelled) mask
// @returns mask or -1 if impossible
// @param distance total distance travelled
int BinaryThrottleScheduler::FindBestMask(const CarState& state, double* distance) {
  if (state.previous_angle() >= 60 - 1e-9 || state.position().angle() >= 60 - 1e-9)
    return -1;

  int best_mask = -1;
  *distance = 0;


  // TODO dont predict that much.
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
      if (fabs(states[now].position().angle()) >= 60 - 1e-9)
        return false;
    }
  }
  return true;
}

void BinaryThrottleScheduler::Log(const game::CarState& state) {
  int i = 0;
  for (int g = 0; g < groups_.size(); g++) {
    std::cout << (int) schedule_[i] << " ";
    for (int t = 0; t < groups_[g]; t++)
      i++;
  }
  std::cout << "(" << state.position().piece() << ")" << std::endl;
}

}  // namespace schedulers
