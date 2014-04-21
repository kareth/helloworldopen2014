#ifndef CPP_GAME_POSITION_H_
#define CPP_GAME_POSITION_H_

#include <string>
#include <iostream>
#include "jsoncons/json.hpp"

namespace game {

class Position {
 public:
  Position();

  double angle() const { return angle_; }
  int piece() const { return piece_index_; }
  double piece_distance() const { return piece_distance_; }

  int start_lane() const { return start_lane_; }
  int end_lane() const { return end_lane_; }

  int lap() const { return lap_; }

  // Returns color of the car
  std::string ParseFromJson(const jsoncons::json& data);

  void set_angle(double angle) { angle_ = angle; }
  void set_piece(double piece) { piece_index_ = piece; }
  void set_piece_distance(double piece_distance) { piece_distance_ = piece_distance; }
  void set_start_lane(double start_lane) { start_lane_ = start_lane; }
  void set_end_lane(double end_lane) { end_lane_ = end_lane; }
  void set_lap(double lap) { lap_ = lap; }

 private:
  double angle_ = -1;
  int piece_index_ = -1;
  double piece_distance_ = -1;

  int start_lane_ = -1;
  int end_lane_ = -1;

  int lap_ = -1;
};

}  // namespace game

#endif  // CPP_GAME_POSITION_H_
