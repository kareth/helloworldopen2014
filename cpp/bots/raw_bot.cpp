#include "bots/raw_bot.h"

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
  const auto& msg_type = msg["msgType"].as<std::string>();
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

// TODO(anyone) just get the color
RawBot::msg_vector RawBot::OnYourCar(const jsoncons::json& data) {
  std::cout << "Server: Your Car" << std::endl;

  bot_->YourCar(/* Car */);
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
  // TODO(anyone) Parse car positions

  return CommandToMsg(bot_->GetMove(/* vektor/mapa pozycji aut */));
}

RawBot::msg_vector RawBot::OnLapFinished(const jsoncons::json& data) {
  std::cout << "Server: Lap Finished" << std::endl;

  // TODO(anyone) just color and times

  bot_->CarFinishedLap(/* Car and results */);
  return ping();
}

RawBot::msg_vector RawBot::OnFinish(const jsoncons::json& data) {
  std::cout << "Server: Finish" << std::endl;

  // TODO(anyone) just color of finished car

  bot_->CarFinishedRace(/* Car */);
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
  std::cout << "Server: Someone crashed" << std::endl;

  // TODO(anyone) Parse crash info

  bot_->CarCrashed(/* Car */);
  return ping();
}

RawBot::msg_vector RawBot::OnSpawn(const jsoncons::json& data) {
  std::cout << "Server: Someone restored from crash" << std::endl;

  // TODO(anyone) Parse spawn info

  bot_->CarSpawned(/* Car */);
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

}  // namespace bots
