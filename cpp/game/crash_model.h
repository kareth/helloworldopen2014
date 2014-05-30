#ifndef CPP_GAME_CRASH_MODEL_H_
#define CPP_GAME_CRASH_MODEL_H_

#include <algorithm>
#include <cmath>
#include <iostream>
#include <string>
#include <vector>

#include "gflags/gflags.h"

DECLARE_double(safe_angle);
DECLARE_bool(print_models);

namespace game {

class CrashModel {
 public:
  CrashModel() {
    guess_safe_angle_ = fmin(50.0, FLAGS_safe_angle - 5.0);
  }

  ~CrashModel() {
    if (FLAGS_print_models) {
      std::cout << "==== Crash Model ====" << std::endl;
      std::cout << "safe_angle: " << safe_angle_ << std::endl;
      std::cout << "unsafe_angle: " << unsafe_angle_ << std::endl;
      std::cout << (ready_ ? "with crash" : "without crash") << std::endl;
      std::cout << std::endl << std::endl;
    }
  }

  void RecordCarCrash(double unsafe_angle) {
    ready_ = true;
    unsafe_angle_ = fmin(unsafe_angle_, fabs(unsafe_angle));
  }

  void RecordSafeAngle(double angle) {
    safe_angle_ = fmax(safe_angle_, fabs(angle));
  }

  void RecordDriftModelReady() {
    guess_safe_angle_ = FLAGS_safe_angle;
  }

  bool IsSafe(double angle) const {
    if (IsReady()) {
      return fabs(angle) < safe_angle_;
    } else {
      return fabs(angle) < fmax(guess_safe_angle_, safe_angle_);
    }
  }

  // Returns true if there was at least one crash.
  bool IsReady() const {
    return ready_;
  }

  double GetModel() const {
      return safe_angle_;
  }

 private:

  // True if the model is ready (there was at least one crash.
  bool ready_ = false;

  // The angle that will not cause crash.
  double guess_safe_angle_;
  double safe_angle_ = 0.0;
  double unsafe_angle_ = 90.0;
};

}  // namespace game

#endif  // CPP_GAME_CRASH_MODEL_H_
