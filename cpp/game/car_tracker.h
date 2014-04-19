#ifndef CPP_GAME_CAR_TRACKER_H_
#define CPP_GAME_CAR_TRACKER_H_

#include <iostream>
#include <fstream>
#include <string>
#include <map>

#include "game/race.h"
#include "game/position.h"
#include "game/gauss.h"

using std::map;

namespace game {

class ErrorTracker {
 public:
  void Add(double error) {
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
  double error_max_ = 0.0;
  double error_sum_ = 0.0;
  int error_count_ = 0;
};


class SingleDriftModel {
 public:
  SingleDriftModel(const vector<double>& x, double radius) : x_(x), radius_(radius) {
  }

  void Record(double angle, double previous_angle,
              double previous_previous_angle, double previous_velocity) {
    error_tracker_.Add(angle, Predict(previous_angle, previous_previous_angle, previous_velocity));
  }

  double Predict(double angle, double previous_angle, double velocity) {
    return x_[0] * angle +
           x_[1] * previous_angle +
           x_[2] * velocity * velocity * cos(rad(angle)) / R(angle) +
           x_[3] * sin(rad(angle)) +
           x_[4] * velocity * velocity * sin(rad(angle)) * sin(rad(angle)) / R(angle) / R(angle) +
           x_[5] * velocity * velocity / R(angle) * sqrt(1.0 - sin(rad(angle)) * sin(rad(angle)) / R(angle) / R(angle)) +
           x_[6] * cos(rad(angle));
  }

  double Accuracy() {
    return error_tracker_.Average();
  }

  void PrintAccuracy() {
    error_tracker_.Print();
  }

  const vector<double>& x() const { return x_; }

 private:
  double rad(double deg) { return deg * M_PI / 180.0; }

  double R(double angle) {
    if (radius_ < 1e-5 && radius_ > -1e-5) return 2000000000;
    double l = 10.0;
    return sqrt(radius_ * radius_ + l * l - 2.0 * l * radius_ * cos(rad(90.0 + angle)));
  }

  double radius_;
  vector<double> x_;
  ErrorTracker error_tracker_;
};

// We assume following drift model
//
// angle = x0 * previous_angle + x1 * previous_previous_angle + x2 * previous_velocity + x3
class DriftModel {
 public:
  DriftModel() {}
  explicit DriftModel(double radius) : radius_(radius), models_() {

    //TODO hardcoded for this track
    if (radius_ < -1e-5) real_radius_ = -radius_ - 10;
    else if (radius_ > 1e-5) real_radius_ = radius + 10;

    char filename[50];
    sprintf (filename, "bin/drift.%lf.csv", radius);
    file_.open (filename);
    file_ << "p_angle,p_p_angle,p_velocity,angle" << std::endl;
  }

  ~DriftModel() {
    std::cout << "==== Drift Model ====" << std::endl;
    std::cout << "radius: " << radius_ << std::endl;
    if (IsReady()) {
      for (int i = 0; i < model_size_; i++)
        std::cout << "x" << i <<": " << x_[i] << " ";
      std::cout << std::endl;
    }
    error_tracker_.Print();
    models_[PickBestModel()]->PrintAccuracy();

    std::cout << std::endl;

    for (int i = 0; i < models_.size(); i++)
      delete models_[i];

    file_.close();
  }

  void Record(double angle, double previous_angle,
              double previous_previous_angle, double previous_velocity) {
    if (angle == 0) return;

    file_ << previous_angle << "," << previous_previous_angle << "," << previous_velocity << "," << angle;

    if (IsReady()) {
      double predicted = Predict(previous_angle, previous_previous_angle, previous_velocity);
      file_ << "," << predicted - angle;
      error_tracker_.Add(angle, predicted);
    } file_ << std::endl;

    // Update models accuracy
    for (auto& model : models_)
      model->Record(angle, previous_angle, previous_previous_angle, previous_velocity);

    data_.push_back({previous_velocity});
    m_.push_back({
        previous_angle,
        previous_previous_angle,
        previous_velocity * previous_velocity * cos(rad(previous_angle)) /  R(previous_angle),
        sin(rad(previous_angle)),
        previous_velocity * previous_velocity * sin(rad(previous_angle)) * sin(rad(previous_angle)) / R(previous_angle) / R(previous_angle),
        previous_velocity * previous_velocity / R(previous_angle) * sqrt(1.0 - sin(rad(previous_angle)) * sin(rad(previous_angle)) / R(previous_angle) / R(previous_angle)),
        cos(rad(previous_angle)),
        1
        });
    b_.push_back(angle);

    if (m_.size() >= model_size_) {
      AddNewModel();
      PickBestModel();
    }
  }

  double Predict(double angle, double previous_angle, double velocity) {
    return
      x_[0] * angle +  // 2 kolejne potrzebne do przyspieszenia kątowego
      x_[1] * previous_angle + // jw.
      x_[2] * velocity * velocity * cos(rad(angle)) / R(angle) + // Siła odśrodkowa
      x_[3] * sin(rad(angle)) +  // Siła oporu związana z obrotem
      x_[4] * velocity * velocity * sin(rad(angle)) * sin(rad(angle)) / R(angle) / R(angle) +
      x_[5] * velocity * velocity / R(angle) * sqrt(1.0 - sin(rad(angle)) * sin(rad(angle)) / R(angle) / R(angle)) +
      x_[6] * cos(rad(angle));
  }

  bool IsReady() {
    return ready_;
  }

 private:
  double R(double angle) {
    if (radius_ < 1e-5 && radius_ > -1e-5) return 2000000000;
    double l = 10.0;
    return sqrt(real_radius_ * real_radius_ + l * l - 2.0 * l * real_radius_ * cos(rad(90.0 + angle)));
  }

  void AddNewModel() {
    vector<double> x;
    vector<double> b;
    vector<vector<double>> m;
    for (int i = m_.size() - model_size_; i < m_.size(); i++) {
      m.push_back(m_[i]);
      b.push_back(b_[i]);
    }
    GaussDouble(m, b, x);
    models_.push_back(new SingleDriftModel(x, real_radius_));

    for (int i = 0; i < m_.size(); i++)
      models_.back()->Record(b_[i], m_[i][0], m_[i][1], data_[i][0]);

    if (m_.size() >= model_size_ + 1)
      ready_ = true;
  }

  int PickBestModel() {
    int best = 0;
    for (int i = 0; i < models_.size(); i++)
      if (models_[i]->Accuracy() < models_[best]->Accuracy())
        best = i;
    x_ = models_[best]->x();
    return best;
  }
  const int model_size_ = 7;

  double rad(double deg) { return deg * M_PI / 180.0; }

  double radius_ = 0.0;
  double real_radius_;
  std::ofstream file_;

  bool ready_ = false;
  // {previous_angle, previous_previous_angle, previous_velocity, angle}
  vector<vector<double> > m_;
  vector<vector<double> > data_;
  vector<double> b_;

  vector<double> x_;
  ErrorTracker error_tracker_;

  vector<SingleDriftModel*> models_;
};

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


class CrashModel {
 public:
  ~CrashModel() {
    std::cout << "==== Crash Model ====" << std::endl;
    std::cout << "threshold: " << angle_threshold_ << std::endl;
    std::cout << (ready_ ? "with crash" : "without crash") << std::endl;
    std::cout << "Crashes: ";
    for (double c : crashes_)
      std::cout << c << ", ";
    std::cout << std::endl << std::endl;
  }

  void RecordCarCrash(double angle) {
    ready_ = true;
    angle_threshold_ = fmax(angle_threshold_, angle);
    crashes_.push_back(angle);
  }

  void Record(double angle) {
    angle_threshold_ = fmax(angle_threshold_, angle);
  }

  // WillCrash?
  bool Predict(double angle) const {
    if (ready_) return false;
    return angle < angle_threshold_;
  }

  bool IsReady() const {
    return ready_;
  }

 private:

  // The angle that will not cause crash.
  // HACK(tomek) Hardcoded for finland track. Change to false and 0.
  bool ready_ = false;
  double angle_threshold_ = 0.0;
  vector<double> crashes_;
};

template <typename T> int sgn(T val) {
    return (T(0) < val) - (val < T(0));
}

class CarTracker {
 public:
  CarTracker(const Race* race) : race_(race) {
    stats_file_.open ("bin/stats.csv");
    stats_file_ << "piece_index,in_piece_distance,angle,velocity" << std::endl;
  }

  ~CarTracker() {
    stats_file_.close();
  }

  void Record(const Position& position) {
    // Note: all variables with _ are old e.g.:
    // velocity_ - previous velocity
    // velocity  - new velocity

    // Calculate new values for state - velocity, angle, etc.
    double angle = position.angle();
    double velocity = velocity_;
    if (positions_.size() > 0) {
      const auto& prev_position = positions_.back();

      if (prev_position.piece() == position.piece()) {
        velocity = position.piece_distance() - prev_position.piece_distance();
      } else {
        velocity = velocity_model_.Predict(velocity_, throttle_);
      }
    }

    // TODO do not update model after crash

    // For some reason first few entries are bogus. Do not train model on them.
    // Update Models
    crash_model_.Record(angle);
    velocity_model_.Record(velocity, velocity_, throttle_);
    // TODO last position
    auto pos = position;
    if (positions_.size() > 0) pos = positions_.back();
    GetDriftModel(pos)->Record(angle, angle_, previous_angle_, velocity_);

    // Update state
    previous_angle_ = angle_;
    angle_ = angle;
    velocity_ = velocity;
    positions_.push_back(position);

    LogState();
  }

  DriftModel* GetDriftModel(const Position& position) {
    auto& piece = race_->track().pieces().at(position.piece());
    double radius = piece.radius() * sgn(piece.angle());
    if (drift_model_[radius] == nullptr) {
      drift_model_[radius].reset(new DriftModel(radius));
    }
    return drift_model_[radius].get();
  }

  void RecordThrottle(double throttle) {
    throttle_ = throttle;
  }

  void LogState() {
    const auto& position = positions_.back();

    stats_file_ << position.piece() << ","
                << position.piece_distance() << ","
                << angle_ << ","
                << velocity_ << std::endl;
  }

  void RecordCarCrash() {
    crash_model_.RecordCarCrash(angle_);

    velocity_ = 0;
    angle_ = 0;

    stats_file_ << "CRASH" << std::endl;
  }

  double velocity() const { return velocity_; }

  double angle() const { return angle_; }

 private:
  std::ofstream stats_file_;

  double velocity_ = 0;
  double angle_ = 0;
  double previous_angle_ = 0;

  const Race* race_;

  CrashModel crash_model_;
  VelocityModel velocity_model_;
  // We need separate drift models for different pieces. We group them by
  // radius (straight pieces assume radius 0).
  map<double, std::unique_ptr<DriftModel>> drift_model_;

  vector<Position> positions_;
  double throttle_ = 0;
};


}  // namespace game

#endif  // CPP_GAME_CAR_TRACKER_H_
