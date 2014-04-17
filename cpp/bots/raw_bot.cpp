#include "bots/raw_bot.h"

#include "game/position.h"

namespace bots {

RawBot::RawBot(BotInterface* bot)
  : bot_(bot), action_map_ {
      { "join", &RawBot::OnJoin },
      { "yourCar", &RawBot::OnYourCar },
      { "gameInit", &RawBot::OnGameInit },
      { "gameStart", &RawBot::OnGameStart },
      { "carPositions", &RawBot::OnCarPositions },
      { "lapFinished", &RawBot::OnLapFinished },
      { "finish", &RawBot::OnFinish },
      { "gameEnd", &RawBot::OnGameEnd },
      { "tournamentEnd", &RawBot::OnTournamentEnd },
      { "crash", &RawBot::OnCrash },
      { "spawn", &RawBot::OnSpawn },
      { "error", &RawBot::OnError },
      { "dnf", &RawBot::OnDNF }
    }
{
}

RawBot::msg_vector RawBot::CommandToMsg(const game::Command& command) {
  msg_vector result;

  if (command.SwitchSet())
    result.push_back(utils::make_switch(command.get_switch()));

  if (command.ThrottleSet())
    result.push_back(utils::make_throttle(command.get_throttle()));

  if (result.size() == 0)
    result.push_back(utils::make_ping());

  return result;
}

RawBot::msg_vector RawBot::React(const jsoncons::json& msg) {
  const auto& msg_type = msg["msgType"].as_string();
  const auto& data = msg["data"];
  auto action_it = action_map_.find(msg_type);
  if (action_it != action_map_.end()) {
    return (action_it->second)(this, data);
  } else {
    std::cout << "Unknown message type: " << msg_type << std::endl;
    return { utils::make_ping() };
  }
}

RawBot::msg_vector RawBot::OnJoin(const jsoncons::json& data) {
  std::cout << "Server: Join" << std::endl;
  bot_->JoinedGame();
  return ping();
}

RawBot::msg_vector RawBot::OnYourCar(const jsoncons::json& data) {
  const auto& color = data["color"].as_string();
  std::cout << "Server: Your Car - " << ColorPrint(color) << std::endl;

  bot_->YourCar(color);
  return ping();
}

RawBot::msg_vector RawBot::OnGameInit(const jsoncons::json& data) {
  std::cout << "Server: Game Init" << std::endl;

  // TODO(anyone) very important, parse track info

  bot_->NewRace(/* Race */);
  return ping();
}

RawBot::msg_vector RawBot::OnGameStart(const jsoncons::json& data) {
  std::cout << "Server: Game start" << std::endl;

  bot_->GameStarted();
  return ping();
}

RawBot::msg_vector RawBot::OnCarPositions(const jsoncons::json& data) {
  std::map<std::string, game::Position> positions;
  for (auto it = data.begin_elements(); it != data.end_elements(); ++it) {
    game::Position position;
    auto color = position.ParseFromJson(*it);
    positions[color] = position;
  }

  return CommandToMsg(bot_->GetMove(positions));
}

RawBot::msg_vector RawBot::OnLapFinished(const jsoncons::json& data) {
  const auto& color = data["car"]["color"].as_string();
  std::cout << "Server: " << ColorPrint(color) << " Finished lap" << std::endl;

  // TODO(anyone) times

  bot_->CarFinishedLap(color /* + results */);
  return ping();
}

RawBot::msg_vector RawBot::OnFinish(const jsoncons::json& data) {
  const auto& color = data["color"].as_string();
  std::cout << "Server: " << ColorPrint(color) << " finished the race" << std::endl;

  bot_->CarFinishedRace(color);
  return ping();
}

RawBot::msg_vector RawBot::OnGameEnd(const jsoncons::json& data) {
  std::cout << "Server: Game end" << std::endl;

  // TODO(anyone) Parge gmae results

  bot_->GameEnd(/* results */);
  return ping();
}

RawBot::msg_vector RawBot::OnTournamentEnd(const jsoncons::json& data) {
  std::cout << "Server: Tournament end" << std::endl;
  bot_->TournamentEnd();
  return ping();
}

RawBot::msg_vector RawBot::OnCrash(const jsoncons::json& data) {
  const auto& color = data["color"].as_string();
  std::cout << "Server: " << ColorPrint(color) << " player crashed" << std::endl;

  bot_->CarCrashed(color);
  return ping();
}

RawBot::msg_vector RawBot::OnSpawn(const jsoncons::json& data) {
  const auto& color = data["color"].as_string();
  std::cout << "Server: " << ColorPrint(color) << " restored from crash" << std::endl;

  bot_->CarSpawned(color);
  return ping();
}

RawBot::msg_vector RawBot::OnError(const jsoncons::json& data) {
  std::cout << "Server: Error - " << data.to_string() << std::endl;
  return ping();
}

RawBot::msg_vector RawBot::OnDNF(const jsoncons::json& data) {
  std::cout << "Server: Disqualification - " << data["reason"].as<std::string>() << std::endl;
  return ping();
}

std::string RawBot::ColorPrint(const std::string& color) const {
  std::map<std::string, std::string> colors {
    { "red", "\x1B[31m" },
    { "blue", "\x1B[34m" },
    { "green", "\x1B[32m" },
    { "normal", "\x1B[0m" }
  };

  if (colors.find(color) != colors.end())
    return colors[color] + color + colors["normal"];
  else
    return color;
}

}  // namespace bots
