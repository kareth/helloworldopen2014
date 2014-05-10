#include <iostream>

#include "game/track.h"
#include "jsoncons/json.hpp"
#include <algorithm>
#include <fstream>
#include <string>


using jsoncons::json;

namespace game {

namespace {
Piece ParsePiece(const json& data) {
  // Is it a straight piece?
  if (data.has_member("length")) {
    return Piece::CreateStraight(
          data.get("length").as_double(),
          data.get("switch", json(false)).as_bool());
  }

  // Otherwise it is a bent piece.
  return Piece::CreateBent(
      data.get("angle").as_double(),
      data.get("radius").as_double(),
      data.get("switch", json(false)).as_bool());
}
Lane ParseLane(const json& data) {
  return Lane(data.get("index").as_int(),
              data.get("distanceFromCenter").as_double());
}
}  // anonymous namespace

void Track::ParseFromJson(const json& data) {
  id_ = data["id"].as_string();
  name_ = data["name"].as_string();

  for (auto it = data["pieces"].begin_elements(); it != data["pieces"].end_elements(); ++it) {
    pieces_.push_back(ParsePiece(*it));
  }

  for (auto it = data["lanes"].begin_elements(); it != data["lanes"].end_elements(); ++it) {
    lanes_.push_back(ParseLane(*it));
  }
}

const Piece& Track::PieceFor(const Position& position, int offset) const {
  int index = (position.piece() + offset + pieces_.size()) % pieces_.size();
  return pieces_.at(index);
}

double Track::LaneRadius(int piece, int lane) const {
  if (pieces_[piece].radius() < 1e-5)
    return 0;

  if (pieces_[piece].angle() > 1e-5)
    return pieces_[piece].radius() - lanes_[lane].distance_from_center();
  else
    return pieces_[piece].radius() + lanes_[lane].distance_from_center();
}

double Track::LaneLength(int piece, int lane) const {
  if (pieces_[piece].radius() < 1e-5)
    return pieces_[piece].length();

  double radius = LaneRadius(piece, lane);
  double angle = pieces_[piece].angle();
  return 2.0 * M_PI * radius * (fabs(angle) / 360.0);
}

double Track::LaneLength(const Position& position) const {
  const auto& piece = pieces_[position.piece()];

  if (piece.type() == PieceType::kStraight) {
    if (position.start_lane() == position.end_lane()) {
      return piece.length();
    }
    if (!piece.has_switch()) {
      std::cerr << "Changing lane on non switch piece?" << std::endl;
    }
    // TODO this is just approximation. Add learning.

    double width = fabs(lanes_[position.start_lane()].distance_from_center() - lanes_[position.end_lane()].distance_from_center());
    return 1.000783334 * std::sqrt(width * width + piece.length() * piece.length());
  }

  if (position.start_lane() == position.end_lane()) {
    double radius = LaneRadius(position.piece(), position.start_lane());
    return 2.0 * M_PI * radius * (fabs(piece.angle()) / 360.0);
  }

  if (!piece.has_switch()) {
    std::cerr << "Changing lane on non switch piece?" << std::endl;
  }

  // TODO this is just approximation. Add learning.
  double radius1 = LaneRadius(position.piece(), position.start_lane());
  double radius2 = LaneRadius(position.piece(), position.end_lane());
  return 1.05 * M_PI * radius1 * (fabs(piece.angle()) / 360.0) + M_PI * radius2 * (fabs(piece.angle()) / 360.0);
}

bool Track::IsLastStraight(const Position& position) const {
  for (int i = pieces_.size() - 1; i >= position.piece(); i--)
    if (pieces_[i].type() == PieceType::kBent)
      return false;
  return true;
}

// TODO test
Position Track::PositionAfter(const Position& start, double distance) const {
  auto position = start;
  position.set_piece_distance(position.piece_distance() + distance);
  if (position.piece_distance() > LaneLength(position)) {
    position.set_piece_distance(position.piece_distance() - LaneLength(position));
    position.set_piece(position.piece());
  }
  return position;
}

bool Track::IsFirstInFront(const Position& front, const Position& back) const {
  int left, right;
  if (front.piece() >= back.piece()) {
    left = front.piece() - back.piece();
    right = back.piece() + pieces_.size() - front.piece();
  } else {
    right = back.piece() - front.piece();
    left = front.piece() + pieces_.size() - back.piece();
  }
  if (left == 0)
    return front.piece_distance() - back.piece_distance() > 0;
  return left < right;
}

bool Track::IsBetween(const Position& position, int from, int to) const {
  if (from <= to) {
    return position.piece() >= from && position.piece() <= to;
  } else {
    return position.piece() <= to || position.piece() >= from;
  }
}

int Track::NextSwitch(int piece_index) const {
  int index = 1;
  auto piece = pieces_[(piece_index + 1) % pieces_.size()];

  while (index <= pieces_.size() && piece.has_switch() == false) {
    index++;
    piece = pieces_[(piece_index + index) % pieces_.size()];
  }
  return (piece_index + index) % pieces_.size();
}

}  // namespace game
