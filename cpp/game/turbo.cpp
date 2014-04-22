#include "game/turbo.h"

namespace game {

void Turbo::ParseFromJson(const jsoncons::json& data) {
  factor_ = data.get("turboFactor").as_double();
  ticks_ = data["turboDurationTicks"].as_int();
}

}  // namespace game
