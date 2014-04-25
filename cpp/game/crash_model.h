#ifndef CPP_GAME_CRASH_MODEL_H_
#define CPP_GAME_CRASH_MODEL_H_

#include <algorithm>
#include <cmath>
#include <iostream>
#include <string>
#include <vector>

namespace game {

class CrashModel {
 public:
  ~CrashModel() {
    std::cout << "==== Crash Model ====" << std::endl;
    std::cout << "safe_angle: " << safe_angle_ << std::endl;
    std::cout << "unsafe_angle: " << unsafe_angle_ << std::endl;
    std::cout << (ready_ ? "with crash" : "without crash") << std::endl;
    std::cout << std::endl << std::endl;
  }

  void RecordCarCrash(double unsafe_angle) {
    ready_ = true;
    unsafe_angle_ = fmin(unsafe_angle_, fabs(unsafe_angle));
  }

  void RecordSafeAngle(double angle) {
    safe_angle_ = fmax(safe_angle_, fabs(angle));
  }

  bool IsSafe(double angle) const {
    if (IsReady()) {
      return fabs(angle) < safe_angle_;
    } else {
      return fabs(angle) < safe_angle_ + 10;
    }
  }

  // Returns true if there was at least one crash.
  bool IsReady() const {
    return ready_;
  }

 private:

  // True if the model is ready (there was at least one crash.
  bool ready_ = false;

  // The angle that will not cause crash.
  double safe_angle_ = 0.0;
  double unsafe_angle_ = 90.0;
};

}  // namespace game

#endif  // CPP_GAME_CRASH_MODEL_H_
