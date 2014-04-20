#include <iostream>

#include "game/track.h"
#include "jsoncons/json.hpp"

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

  return pieces_[piece].radius() - lanes_[lane].distance_from_center();
}

double Track::LaneLength(int piece, int lane) const {
  if (pieces_[piece].radius() < 1e-5)
    return pieces_[piece].length();

  double radius = LaneRadius(piece, lane);
  double angle = pieces_[piece].angle();
  return 2.0 * M_PI * radius * (fabs(angle) / 360.0);
}

}  // namespace game
