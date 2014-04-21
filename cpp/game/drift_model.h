#ifndef CPP_GAME_DRIFT_MODEL_H_
#define CPP_GAME_DRIFT_MODEL_H_

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <algorithm>

#include "game/error_tracker.h"
#include "game/gauss.h"
#include "game/position.h"
#include "game/race.h"
#include "game/simplex.h"

namespace game {

class SingleDriftModel {
 public:
  SingleDriftModel(const vector<double>& x) : x_(x) {
  }

  void Record(double angle, double previous_angle,
              double previous_previous_angle, double previous_velocity, double radius) {
    error_tracker_.Add(angle, Predict(previous_angle, previous_previous_angle, previous_velocity, radius));
  }

  double Predict(double angle, double previous_angle, double velocity, double radius) {
    double Radius = R(angle, radius);
    return x_[0] * angle +
           x_[1] * previous_angle +
           x_[2] * velocity * angle +
           x_[3] * velocity * fmax(0, 424.2017117 * velocity / sqrt(Radius) - 239.9595605);
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
    return radius;
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
      //PickBestModel();
    }
  }

  void SaveEntry(double angle, double previous_angle, double previous_previous_angle, double previous_velocity, double radius) {
    data_.push_back({previous_velocity, radius});
    double Radius = R(previous_angle, radius);
    m_.push_back({
        previous_angle +
        previous_previous_angle +
        previous_velocity * previous_angle,
        previous_velocity * fmax(0, 424.2017117 * previous_velocity / sqrt(Radius) - 239.9595605)
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

  void AddModel(const vector<double> &x) {
    // TODO kareth
    models_.push_back(new SingleDriftModel(x));
    best_model_ = models_.back();
    manual_ = true;
  }

 private:
  double R(double angle, double radius) {
    if (radius < 1e-5 && radius > -1e-5) return 2000000000;
    return radius;
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
        //Simplex::Optimize(m, b, x);
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
    if (manual_) return;
    for (int i = 0; i < models_.size(); i++)
      if (best_model_ == nullptr || models_[i]->Accuracy() < best_model_->Accuracy())
        best_model_ = models_[i];
  }

  const int model_size_ = 4;
  const int model_window_ = 4; // Max Amount of recent data used for simplex

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
  int manual_ = false;
};

}  // namespace game

#endif  // CPP_GAME_DRIFT_MODEL_H_
