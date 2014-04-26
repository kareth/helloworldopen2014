#ifndef CPP_GAME_ERROR_TRACKER_H_
#define CPP_GAME_ERROR_TRACKER_H_

#include <iostream>
#include <algorithm>
#include <cmath>

namespace game {

class ErrorTracker {
 public:
  ErrorTracker(){}
  ErrorTracker(const std::string& name) : name_(name) {}

  void Add(double error) {
    if (error > 1e-5) {
      std::cout << "Big error in " << name_ << " (" << error << ")" << std::endl;
    }
    error_max_ = fmax(error, error_max_);
    error_sum_ += error;
    error_count_++;
  }

  void Add(double predicted, double actual) {
    Add(fabs(predicted - actual));
  }

  void Print() {
    std::cout << "Error Avg: " << (error_sum_ / error_count_) << " Max: " << error_max_ << std::endl;
  }

  double Average() {
    return error_sum_ / error_count_;
  }

 private:
  std::string name_;
  double error_max_ = 0.0;
  double error_sum_ = 0.0;
  int error_count_ = 0;
};

}  // namespace game

#endif  // CPP_GAME_ERROR_TRACKER_H_
