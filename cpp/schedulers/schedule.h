#ifndef CPP_SCHEDULERS_SCHEDULE_H_
#define CPP_SCHEDULERS_SCHEDULE_H_

#include <vector>
#include <array>
#include <cmath>
#include "game/car_tracker.h"

namespace schedulers {

class Sched {
 public:
  Sched(game::CarTracker* car_tracker_, int horizon);
  size_t size() const { return throttles_.size(); }
  double distance() const { return distance_; }
  vector<double> throttles() const { return throttles_; }
  void set_distance(double distance) { distance_ = distance; }

  game::CarState Predict(const game::CarState& state);
  void UpdateDistance(const game::CarState& state);
  bool IsSafe(const game::CarState& state);
  void ShiftLeft(const game::CarState& state);
  void ShiftLeftFillSafe(const game::CarState& state);
  void ShiftRight(const game::CarState& state, double throttle0);
  void Reset(const game::CarState& state);
  void Print();
  void Widen(const game::CarState& state, int num_values);
  void Shorten(const game::CarState& state, int num_values);

  double& operator[](std::size_t idx) {
    return throttles_[idx];
  };

  const double operator[](std::size_t idx) const {
    return throttles_[idx];
  };

  vector<double> throttles_; // Breaks encapsulation, but need it, unfortunatelly

 private:
  game::CarTracker* car_tracker_;
  double distance_;
};

}

#endif
