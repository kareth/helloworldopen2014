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
    std::cout << "threshold: " << angle_threshold_ << std::endl;
    std::cout << (ready_ ? "with crash" : "without crash") << std::endl;
    if (crashes_.size() > 0) {
      std::cout << "Crashes: ";
      for (double c : crashes_)
        std::cout << c << ", ";
    }
    std::cout << std::endl << std::endl;
  }

  void RecordCarCrash(double angle) {
    ready_ = true;
    angle_threshold_ = fmax(angle_threshold_, angle);
    crashes_.push_back(angle);
  }

  // TODO(zurkowski) Do we need this?
  void Record(double angle) {
    angle_threshold_ = fmax(angle_threshold_, angle);
  }

  // Returns the angle that is definitely safe. If the car has higher angle
  // than this value, then it is possible it will crash.
  double safe_angle() const {
    return angle_threshold_;
  }

  // WillCrash?
  bool Predict(double angle) const {
    if (ready_) return false;
    return angle < angle_threshold_;
  }


  // Returns true if there was at least one crash.
  bool IsReady() const {
    return ready_;
  }

 private:

  // True if the model is ready (there was at least one crash.
  bool ready_ = false;

  // The angle that will not cause crash.
  double angle_threshold_ = 0.0;

  // The list of angles just before the crash.
  std::vector<double> crashes_;
};

}  // namespace game

#endif  // CPP_GAME_CRASH_MODEL_H_
