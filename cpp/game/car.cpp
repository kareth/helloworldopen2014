#include "game/car.h"

using jsoncons::json;

namespace game {

void Car::ParseFromJson(const json& data) {
  name_ = data["id"]["name"].as_string();
  color_ = data["id"]["color"].as_string();

  length_ = data["dimensions"]["length"].as_double();
  width_ = data["dimensions"]["width"].as_double();
  guide_flag_position_ = data["dimensions"]["guideFlagPosition"].as_double();
}

}  // namespace game
