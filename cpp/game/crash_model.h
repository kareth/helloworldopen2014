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
    safe_angle_ = 50.0;
  }

  ~CrashModel() {
    if (FLAGS_print_models) {
      std::cout << "==== Crash Model ====" << std::endl;
      std::cout << "safe_angle: " << safe_angle_ << std::endl;
      std::cout << (ready_ ? "with crash" : "without crash") << std::endl;
      std::cout << std::endl << std::endl;
    }
  }

  void RecordCarCrash(double unsafe_angle) {
    ready_ = true;
  }

  void RecordSafeAngle(double angle) {
  }

  void RecordDriftModelReady() {
    safe_angle_ = 59.99;
  }

  bool IsSafe(double angle) const {
    return fabs(angle) < safe_angle_;
  }

  // Returns true if there was at least one crash.
  bool IsReady() const {
    return true;
  }

  double GetModel() const {
    return safe_angle_;
  }

  void force_angle(double safe_angle) {
    safe_angle_ = safe_angle;
  }

 private:

  // True if the model is ready (there was at least one crash.
  bool ready_ = false;

  // The angle that will not cause crash.
  double safe_angle_ = 0.0;
};

}  // namespace game

#endif  // CPP_GAME_CRASH_MODEL_H_
