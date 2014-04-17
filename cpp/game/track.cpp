#include <iostream>

#include "game/track.h"
#include "jsoncons/json.hpp"

namespace game {

void Track::ParseFromJson(const jsoncons::json& data) {
  // std::cout << pretty_print(data);

  id_ = data["id"].as_string();
  name_ = data["name"].as_string();

  // for (auto it = data["pieces"].begin_members();
  //      it != data["pieces"].end_members(); ++it) {
  //   std::cout << pretty_print(it->value());
  //   pieces_.push_back(Piece());
  // }
}

}  // namespace game
