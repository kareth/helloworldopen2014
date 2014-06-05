#include "schedulers/binary_throttle_scheduler.h"
#include "utils/deadline.h"

#include <chrono>

#include "gflags/gflags.h"

DEFINE_bool(log_masks, true, "");

namespace schedulers {

using game::CarState;

BinaryThrottleScheduler::BinaryThrottleScheduler(const game::Race& race,
    game::CarTracker& car_tracker)
  : race_(race), car_tracker_(car_tracker), groups_() {
 // State with 16 groups, takes average 150ms, never exceeding 300ms
 // Each decrease reduces that time 2 times
 // Size = 16 seems quite optimal (1x1 + 15x2)

 int size = 17;
 int time_limit = 10;

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

void BinaryThrottleScheduler::Schedule(const game::CarState& state, int game_tick, const utils::Deadline& deadline) {
  // TODO(kareth) strategies
  if (strategy_ == Strategy::kOptimizeCurrentLap &&
      race_.track().IsLastStraight(state.position())) {
    schedule_ = { 1 };
  } else {
    // auto start = std::chrono::system_clock::now();

    Optimize(state);

    /* auto end = std::chrono::system_clock::now();
    auto elapsed = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);
    std::cout << "time: " << elapsed.count() << std::endl;*/

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
    for (int i = 0; i < schedule_.size(); i++)
      schedule_[i] = 0;
}

int BinaryThrottleScheduler::FindBestMask(const CarState& state, double* distance, int from) {
  if (from >= groups_.size()) { // Juz nic nie ma
    *distance = 0;
    return 0;
  }

  *distance = -1;
  int mask = -1;

  for (int throttle = 0; throttle <= 1; throttle++) {
    bool fail = false;
    vector<CarState> next { state };

    double tick_distance = 0;
    for (int tick = 0; tick < groups_[from]; tick++) {
      next.push_back(car_tracker_.Predict(next.back(), game::Command(throttle)));

      tick_distance += car_tracker_.DistanceBetween(next[next.size()-2].position(), next.back().position());
      if (!car_tracker_.crash_model().IsSafe(next.back().position().angle())) {
        fail = true;
        break;
      }
    }

    if (fail)
      continue;

    double remaining_distance = 0;
    int remaining_mask = FindBestMask(next.back(), &remaining_distance, from + 1);

    if (remaining_mask != -1 &&
        remaining_distance + tick_distance > *distance) {
      *distance = remaining_distance + tick_distance;
      mask = (remaining_mask << 1) | (throttle);
    }
  }

  return mask;
}

void BinaryThrottleScheduler::Log(const game::CarState& state) {
  if (!FLAGS_log_masks) return;
  int i = 0;
  for (int g = 0; g < groups_.size(); g++) {
    if (g >= schedule_.size()) break;
    std::cout << (int) schedule_[i] << " ";
    for (int t = 0; t < groups_[g]; t++)
      i++;
  }
  std::cout << "(" << state.position().piece() << ")" << " angle: " <<
    state.position().angle() << " velocity: " << state.velocity() <<
    std::endl;
}

}  // namespace schedulers
