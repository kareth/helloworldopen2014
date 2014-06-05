#ifndef CPP_GAME_POSITION_H_
#define CPP_GAME_POSITION_H_

#include <string>
#include <iostream>
#include <sstream>

#include "jsoncons/json.hpp"

namespace game {

class Position {
 public:
  Position();
  Position(int piece, double piece_distance)
    : piece_index_(piece), piece_distance_(piece_distance) {}

  double angle() const { return angle_; }
  int piece() const { return piece_index_; }
  double piece_distance() const { return piece_distance_; }

  int start_lane() const { return start_lane_; }
  int end_lane() const { return end_lane_; }

  int lap() const { return lap_; }
  int last_tick() const { return last_tick_; }

  // Returns color of the car
  std::string ParseFromJson(const jsoncons::json& data);

  void set_angle(double angle) { angle_ = angle; }
  void set_piece(double piece) { piece_index_ = piece; }
  void set_piece_distance(double piece_distance) { piece_distance_ = piece_distance; }
  void set_start_lane(double start_lane) { start_lane_ = start_lane; }
  void set_end_lane(double end_lane) { end_lane_ = end_lane; }
  void set_lap(double lap) { lap_ = lap; }

  std::string DebugString() const {
    std::stringstream ss;
    ss << "piece: " << piece_index_ << std::endl;
    ss << "in_piece_distance: " << piece_distance_ << std::endl;
    ss << "angle: " << angle_ << std::endl;
    ss << "start_lane: " << start_lane_ << std::endl;
    ss << "end_lane: " << end_lane_ << std::endl;
    ss << "lap: " << lap_ << std::endl;
    return ss.str();
  }

  std::string ShortDebugString() const {
    std::stringstream ss;
    ss << "piece: " << piece_index_;
    ss << " in_piece_distance: " << piece_distance_;
    ss << " angle: " << angle_;
    ss << " start_lane: " << start_lane_;
    ss << " end_lane: " << end_lane_;
    ss << " lap: " << lap_;
    return ss.str();
  }

 private:
  double angle_ = 0;
  int piece_index_ = 0;
  double piece_distance_ = 0;

  int start_lane_ = 0;
  int end_lane_ = 0;

  int lap_ = 0;
  int last_tick_ = 0;
};

}  // namespace game

#endif  // CPP_GAME_POSITION_H_
