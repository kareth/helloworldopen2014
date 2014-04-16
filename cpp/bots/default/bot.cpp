#include "bots/default/bot.h"

namespace default_bot {

Bot::Bot()
  : action_map {
      { "join", &Bot::on_join },
      { "gameStart", &Bot::on_game_start },
      { "carPositions", &Bot::on_car_positions },
      { "crash", &Bot::on_crash },
      { "gameEnd", &Bot::on_game_end },
      { "error", &Bot::on_error }
    }
{
}

Bot::msg_vector Bot::react(const jsoncons::json& msg) {
  const auto& msg_type = msg["msgType"].as<std::string>();
  const auto& data = msg["data"];
  auto action_it = action_map.find(msg_type);
  if (action_it != action_map.end()) {
    return (action_it->second)(this, data);
  } else {
    std::cout << "Unknown message type: " << msg_type << std::endl;
    return { utils::make_ping() };
  }
}

Bot::msg_vector Bot::on_join(const jsoncons::json& data) {
  std::cout << "Joined" << std::endl;
  return { utils::make_ping() };
}

Bot::msg_vector Bot::on_game_start(const jsoncons::json& data) {
  std::cout << "Race started" << std::endl;
  return { utils::make_ping() };
}

Bot::msg_vector Bot::on_car_positions(const jsoncons::json& data) {
  return { utils::make_throttle(0.6) };
}

Bot::msg_vector Bot::on_crash(const jsoncons::json& data) {
  std::cout << "Someone crashed" << std::endl;
  return { utils::make_ping() };
}

Bot::msg_vector Bot::on_game_end(const jsoncons::json& data) {
  std::cout << "Race ended" << std::endl;
  return { utils::make_ping() };
}

Bot::msg_vector Bot::on_error(const jsoncons::json& data) {
  std::cout << "Error: " << data.to_string() << std::endl;
  return { utils::make_ping() };
}

}  // namespace default_bot
