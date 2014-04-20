#ifndef CPP_GAME_CAR_TRACKER_H_
#define CPP_GAME_CAR_TRACKER_H_

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <algorithm>

#include "game/race.h"
#include "game/position.h"
#include "game/gauss.h"
#include "game/simplex.h"

#define SQR(X) ((X)*(X))

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
  SingleDriftModel(const vector<double>& x) : x_(x) {
  }

  void Record(double angle, double previous_angle,
              double previous_previous_angle, double previous_velocity, double radius) {
    error_tracker_.Add(angle, Predict(previous_angle, previous_previous_angle, previous_velocity, radius));
  }

  double Predict(double angle, double previous_angle, double velocity, double radius) {
    double sine = sin(rad(angle)),
           cosine = cos(rad(angle)),
           Radius = R(angle, radius);
    return x_[0] * angle + // 2 kolejne potrzebne do przyspieszenia kątowego
           x_[1] * previous_angle + // jw.
           x_[2] * SQR(velocity) * cosine / Radius + // Siła odśrodkowa
           x_[3] * sine + // Tarcie na osii x, (z crossem z r)
           x_[4] * SQR(velocity * sine / Radius) + // Odsrodkowa na osii x (z crossem z r)
           // W rownaniu ponizej powinno być jeszcze * cos(rad(angle) ale z jakichs powodow to tylko psuje...
           x_[5] * SQR(velocity) / Radius * cosine * sqrt(1.0 - SQR(10.0 * sine / Radius)) + // odsrodkowa na ossii y (z crossem z r)
           x_[6] * cosine; // Tarcie na osii y (cost) (z crossem z r)
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

  double R(double angle, double radius) {
    if (radius < 1e-5 && radius > -1e-5) return 2000000000;
    double l = 10.0;
    return sqrt(SQR(radius) + SQR(l) - 2.0 * l * radius * cos(rad(90.0 + angle)));
  }

  vector<double> x_;
  ErrorTracker error_tracker_;
};

// We assume following drift model
//
// angle = x0 * previous_angle + x1 * previous_previous_angle + x2 * previous_velocity + x3
class DriftModel {
 public:
  DriftModel() {}
  explicit DriftModel(int direction) : direction_(direction), models_() {
    char filename[50];
    sprintf (filename, "bin/drift.%d.csv", direction);
    file_.open (filename);
    file_ << "p_angle,p_p_angle,p_velocity,angle,radius" << std::endl;
  }

  ~DriftModel() {
    std::cout << "==== Drift Model ====" << std::endl;
    std::cout << "direction: " << direction_ << std::endl;
    if (IsReady()) {
      for (int i = 0; i < model_size_; i++)
        std::cout << "x" << i <<": " << best_model_->x()[i] << " ";
      std::cout << std::endl;
    }
    error_tracker_.Print();
    best_model_->PrintAccuracy();

    std::cout << std::endl;

    for (int i = 0; i < models_.size(); i++)
      delete models_[i];

    file_.close();
  }

  void Record(double angle, double previous_angle, double previous_previous_angle, double previous_velocity, double radius) {
    if (angle == 0) return;

    file_ << previous_angle << "," << previous_previous_angle << "," << previous_velocity << "," << angle << "," << radius;

    if (IsReady()) {
      double predicted = Predict(previous_angle, previous_previous_angle, previous_velocity, radius);
      file_ << "," << predicted - angle;
      error_tracker_.Add(angle, predicted);
    } file_ << std::endl;

    // Update models accuracy
    for (auto& model : models_)
      model->Record(angle, previous_angle, previous_previous_angle, previous_velocity, radius);

    SaveEntry(angle, previous_angle, previous_previous_angle, previous_velocity, radius);

    if (m_.size() >= model_size_) {
      AddNewModel();
      PickBestModel();
    }
  }

  void SaveEntry(double angle, double previous_angle, double previous_previous_angle, double previous_velocity, double radius) {
    data_.push_back({previous_velocity, radius});
    double sine = sin(rad(previous_angle)),
       cosine = cos(rad(previous_angle)),
       Radius = R(previous_angle, radius);

    m_.push_back({
        previous_angle,
        previous_previous_angle,
        SQR(previous_velocity) * cosine / Radius,
        sine,
        SQR(previous_velocity * sine * Radius),
        SQR(previous_velocity) / Radius * cosine * sqrt(1.0 - SQR(10.0 * sine / Radius)),
        cosine
        });

    b_.push_back(angle);
  }

  double Predict(double angle, double previous_angle, double velocity, double radius) {
    if (!IsReady()) return angle;
    return best_model_->Predict(angle, previous_angle, velocity, radius);
  }

  bool IsReady() {
    return ready_;
  }

 private:
  double R(double angle, double radius) {
    if (radius < 1e-5 && radius > -1e-5) return 2000000000;
    double l = 10.0;
    return sqrt(radius * radius + l * l - 2.0 * l * radius * cos(rad(90.0 + angle)));
  }

  void AddNewModel() {
    for (int increment = 1; increment < 20; increment += 5) {
      if (m_.size() >= increment * model_window_) {
        vector<double> x;
        vector<double> b;
        vector<vector<double>> m;
        for (int i = fmax(0, m_.size() - model_window_); i < m_.size(); i += increment) {
          m.push_back(m_[i]);
          b.push_back(b_[i]);
        }
        // Simplex::Optimize(m, b, x);
        GaussDouble(m, b, x);
        models_.push_back(new SingleDriftModel(x));

        for (int i = 0; i < m_.size(); i++)
          models_.back()->Record(b_[i], m_[i][0], m_[i][1], data_[i][0], data_[i][1]);
      }
    }

    if (m_.size() >= model_size_ + 1)
      ready_ = true;
  }

  void PickBestModel() {
    for (int i = 0; i < models_.size(); i++)
      if (best_model_ == nullptr || models_[i]->Accuracy() < best_model_->Accuracy())
        best_model_ = models_[i];
  }

  const int model_size_ = 7;
  const int model_window_ = 7; // Max Amount of recent data used for simplex

  double rad(double deg) { return deg * M_PI / 180.0; }

  int direction_;
  std::ofstream file_;

  bool ready_ = false;
  // {previous_angle, previous_previous_angle, previous_velocity, angle}
  vector<vector<double> > m_;
  vector<vector<double> > data_;
  vector<double> b_;

  ErrorTracker error_tracker_;

  vector<SingleDriftModel*> models_;
  SingleDriftModel* best_model_ = nullptr;
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
    stats_file_ << "piece_index,in_piece_distance,angle,velocity,throttle" << std::endl;
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

    auto& piece = race_->track().pieces().at(pos.piece());
    double radius = abs(piece.radius());
    if (piece.angle() < -1e-5) radius -= 10.0;
    if (piece.angle() > 1e-5) radius += 10.0;

    GetDriftModel(pos)->Record(angle, angle_, previous_angle_, velocity_, radius);

    // Update state
    previous_angle_ = angle_;
    angle_ = angle;
    velocity_ = velocity;
    positions_.push_back(position);

    LogState();
  }

  DriftModel* GetDriftModel(const Position& position) {
    auto& piece = race_->track().pieces().at(position.piece());
    int direction = sgn(piece.angle());

    if (drift_model_[direction] == nullptr) {
      drift_model_[direction].reset(new DriftModel(direction));
    }
    return drift_model_[direction].get();
  }

  void RecordThrottle(double throttle) {
    throttle_ = throttle;
  }

  void LogState() {
    const auto& position = positions_.back();

    stats_file_ << position.piece() << ","
                << position.piece_distance() << ","
                << angle_ << ","
                << velocity_ << ","
                << throttle_ << std::endl;
  }

  void RecordCarCrash() {
    crash_model_.RecordCarCrash(angle_);

    velocity_ = 0;
    angle_ = 0;

    stats_file_ << "CRASH" << std::endl;
  }

  double velocity() const { return velocity_; }

  double angle() const { return angle_; }

  Position Predict(const Position& position, const Position& previous_position, int throttle, bool change_lane) {
    if (!GetDriftModel(position)->IsReady() ||
        !velocity_model_.IsReady())
      return position;

    // TODO swapping lanes prolongs track :D
    // TODO no lane swapping management

    double radius = race_->track().LaneRadius(previous_position.piece(), previous_position.start_lane());
    double velocity;

    if (position.piece() == previous_position.piece())
      velocity = position.piece_distance() - previous_position.piece_distance();
    else
      velocity = position.piece_distance() - previous_position.piece_distance() +
          race_->track().LaneLength(position.piece(), position.start_lane());

    Position result;

    result.set_angle(GetDriftModel(position)->Predict(
        position.angle(),
        previous_position.angle(),
        velocity,
        radius
        ));


    double new_velocity = velocity_model_.Predict(velocity, throttle);
    double new_position = position.piece_distance() + new_velocity;

    int lap = position.lap();

    if (new_position > race_->track().LaneLength(position.piece(), position.start_lane())) {
      int piece = position.piece() + 1;
      if (piece >= race_->track().pieces().size()) {
        piece %= race_->track().pieces().size();
        lap++;
      }
      result.set_piece(piece);
      result.set_piece_distance(new_position - race_->track().LaneLength(position.piece(), position.start_lane()));
    } else {
      result.set_piece(position.piece());
      result.set_piece_distance(new_position);
    }

    result.set_lap(lap);

    // TODO(kareth)
    result.set_start_lane(position.start_lane());
    result.set_end_lane(position.end_lane());

    return result;
  }

  const vector<Position>& positions() { return positions_; }

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
