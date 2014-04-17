#include "game/position.h"

#include <iostream>

namespace game {

Position::Position() {}

std::string Position::ParseFromJson(const jsoncons::json& data) {
  const auto& color = data["id"]["color"].as_string();
  angle_ = data.get("angle").as_double();

  const auto& piece = data.get("piecePosition");

  piece_index_ = piece.get("pieceIndex").as_int();
  piece_distance_ = piece.get("inPieceDistance").as_double();
  start_lane_ = piece.get("lane").get("startLaneIndex").as_int();
  end_lane_ = piece.get("lane").get("endLaneIndex").as_int();
  lap_ = piece.get("lap").as_int();

  return color;
}

}  // namespace game
