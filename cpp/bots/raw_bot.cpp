#include "bots/raw_bot.h"

namespace bots {

RawBot::RawBot(BotInterface* bot)
  : bot_(bot), action_map_ {
      { "join", &RawBot::OnJoin },
      { "gameStart", &RawBot::OnGameStart },
      { "carPositions", &RawBot::OnCarPositions },
      { "crash", &RawBot::OnCrash },
      { "spawn", &RawBot::OnSpawn },
      { "gameEnd", &RawBot::OnGameEnd },
      { "error", &RawBot::OnError },
      { "yourCar", &RawBot::OnYourCar },
      { "gameInit", &RawBot::OnGameInit },
      { "lapFinished", &RawBot::OnLapFinished },
      { "finish", &RawBot::OnFinish }
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

// Nothing to parse
RawBot::msg_vector RawBot::OnJoin(const jsoncons::json& data) {
  std::cout << "Server: Join" << std::endl;
  return CommandToMsg(bot_->OnJoin());
}

// TODO just get the color
RawBot::msg_vector RawBot::OnYourCar(const jsoncons::json& data) {
  std::cout << "Server: Your Car" << std::endl;
  return CommandToMsg(bot_->OnYourCar());
}

//TODO very important, parse track info
RawBot::msg_vector RawBot::OnGameInit(const jsoncons::json& data) {
  std::cout << "Server: Game Init" << std::endl;
  return CommandToMsg(bot_->OnGameInit());
}

// Nothing to parse
RawBot::msg_vector RawBot::OnGameStart(const jsoncons::json& data) {
  std::cout << "Server: Game start" << std::endl;
  return CommandToMsg(bot_->OnGameStart());
}

// TODO Parse car positions
RawBot::msg_vector RawBot::OnCarPositions(const jsoncons::json& data) {
  return CommandToMsg(bot_->OnCarPositions());
}

// TODO just color and times
RawBot::msg_vector RawBot::OnLapFinished(const jsoncons::json& data) {
  std::cout << "Server: Lap Finished" << std::endl;
  return CommandToMsg(bot_->OnLapFinished());
}

// TODO just color of finished car
RawBot::msg_vector RawBot::OnFinish(const jsoncons::json& data) {
  std::cout << "Server: Finish" << std::endl;
  return CommandToMsg(bot_->OnFinish());
}

// TODO Parge gmae results
RawBot::msg_vector RawBot::OnGameEnd(const jsoncons::json& data) {
  std::cout << "Server: Game end" << std::endl;
  return CommandToMsg(bot_->OnGameEnd());
}

// TODO Parse crash info
RawBot::msg_vector RawBot::OnCrash(const jsoncons::json& data) {
  std::cout << "Server: Someone crashed" << std::endl;
  return CommandToMsg(bot_->OnCrash());
}

// TODO Parse spawn info
RawBot::msg_vector RawBot::OnSpawn(const jsoncons::json& data) {
  std::cout << "Server: Someone restored from crash" << std::endl;
  return CommandToMsg(bot_->OnSpawn());
}

//TODO Parse error? do we want it?
RawBot::msg_vector RawBot::OnError(const jsoncons::json& data) {
  std::cout << "Server: Error - " << data.to_string() << std::endl;
  return CommandToMsg(bot_->OnError());
}

}  // namespace default_bot
