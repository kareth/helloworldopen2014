#ifndef CPP_GAME_VELOCITY_MODEL_H_
#define CPP_GAME_VELOCITY_MODEL_H_

namespace game {

// We assume following velocity model
//
// velocity = x0 * previous_velocity + x1 * throttle
class VelocityModel {
 public:
  ~VelocityModel() {
    std::cout << "==== Velocity Model ====" << std::endl;
    std::cout << "x0: " << x_[0] << " x1: " << x_[1] << std::endl;
    error_tracker_.Print();
    std::cout << std::endl;
  }

  // velocity = x * previous_velocity + y * previous_throttle
  void Record(double velocity, double previous_velocity, double previous_throttle) {
    if (velocity == 0) return;

    if (IsReady()) {
      error_tracker_.Add(velocity, Predict(previous_velocity, previous_throttle));
    }

    if (m_.size() < 2) {
      m_.push_back({previous_velocity, previous_throttle});
      b_.push_back(velocity);
    }

    if (m_.size() == 2 && !IsReady()) {
      Train();
    }
  }

  // Returns predicted new velocity based on velocity and applied throttle.
  double Predict(double velocity, double throttle) {
    return x_[0] * velocity + x_[1] * throttle;
  }

  bool IsReady() {
    return ready_;
  }

 private:
  void Train() {
    GaussDouble(m_, b_, x_);
    ready_ = true;
    // std::cout << "\n\n\n\nVelocity: " << x_[0] << " " << x_[1] << "\n\n\n\n";
  }

  bool ready_ = false;

  // Variables used to train the model
  // {previous_velocity, previous_throttle} {velocity}
  vector<vector<double> > m_;
  vector<double> b_;

  // Model used for prediction.
  vector<double> x_;

  ErrorTracker error_tracker_;
};

}  // namespace game

#endif  // CPP_GAME_VELOCITY_MODEL_H_
