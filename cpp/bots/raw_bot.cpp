#include "bots/raw_bot.h"

#include <iostream>
#include <fstream>

#include "game/position.h"
#include "game/race.h"

DEFINE_bool(dump_history, false, "");

using jsoncons::json;

namespace bots {

RawBot::RawBot(BotInterface* bot)
  : bot_(bot), visualizer_(), action_map_ {
      { "join", &RawBot::OnJoin },
      { "joinRace", &RawBot::OnJoin },
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
  history["positions"] = json(json::an_array);
  history["commands"] = json(json::an_array);
}

RawBot::~RawBot() {
  if (FLAGS_dump_history) {
    std::ofstream file;
    file.open("bin/history.json");
    file << pretty_print(history) << std::endl;
    file.close();
  }
}

RawBot::msg_vector RawBot::CommandToMsg(const game::Command& command, int game_tick) {
  msg_vector result;

  if (command.SwitchSet())
    result.push_back(utils::make_switch(command.get_switch()));

  if (command.ThrottleSet())
    result.push_back(utils::make_throttle(command.get_throttle(), game_tick));

  return result;
}

RawBot::msg_vector RawBot::React(const jsoncons::json& msg) {
  const auto& msg_type = msg.get("msgType", jsoncons::json("")).as_string();
  auto action_it = action_map_.find(msg_type);
  if (action_it != action_map_.end()) {
    return (action_it->second)(this, msg);
  } else {
    std::cout << "Unknown message type: " << msg_type << "\n\n\n\n" << std::endl;
    return { utils::make_ping() };
  }
}

RawBot::msg_vector RawBot::OnJoin(const jsoncons::json& msg) {
  TransitionState(State::kWaitingForJoin, State::kJoin);

  std::cout << "Server: Join" << std::endl;
  bot_->JoinedGame();
  return ping();
}

RawBot::msg_vector RawBot::OnYourCar(const jsoncons::json& msg) {
  TransitionState(State::kJoin, State::kYourCar);
  const auto& data = msg.get("data", jsoncons::json(""));

  const auto& color = data["color"].as_string();
  std::cout << "Server: Your Car - " << ColorPrint(color) << std::endl;

  bot_->YourCar(color);
  return ping();
}

RawBot::msg_vector RawBot::OnGameInit(const jsoncons::json& msg) {
  TransitionState(State::kYourCar, State::kGameInit);
  const auto& data = msg.get("data", jsoncons::json(""));

  std::cout << "Server: Game Init" << std::endl;

  game::Race race;
  race.ParseFromJson(data["race"]);

  visualizer_.set_race(race);
  bot_->NewRace(race);
  return ping();
}

RawBot::msg_vector RawBot::OnGameStart(const jsoncons::json& msg) {
  TransitionState(State::kGameInit, State::kGameStart);

  std::cout << "Server: Game start" << std::endl;

  visualizer_.GameStart();
  bot_->GameStarted();
  return ping();
}

RawBot::msg_vector RawBot::OnCarPositions(const jsoncons::json& msg) {
  const auto& data = msg.get("data", jsoncons::json(""));
  const int game_tick = msg.get("gameTick", jsoncons::json(0)).as_int();
  if (game_tick != last_game_tick_ + 1) {
    //std::cerr << "unexpected game tick: " << game_tick << " last: " << last_game_tick_ << std::endl;
    return ping();
  }
  last_game_tick_ = game_tick;

  std::map<std::string, game::Position> positions;
  for (auto it = data.begin_elements(); it != data.end_elements(); ++it) {
    game::Position position;
    auto color = position.ParseFromJson(*it);
    positions[color] = position;
  }

  visualizer_.Update(positions);
  auto command = CommandToMsg(bot_->GetMove(positions, game_tick), game_tick);

  if (FLAGS_dump_history) {
    history["positions"].add(data[0]);
    history["commands"].add(command);
  }

  return command;
}

RawBot::msg_vector RawBot::OnLapFinished(const jsoncons::json& msg) {
  const auto& data = msg.get("data", jsoncons::json(""));

  game::Result result;
  result.ParseFromJson(data);

  visualizer_.LapFinished(result);

  bot_->CarFinishedLap(result.color() /* + results */);
  return ping();
}

RawBot::msg_vector RawBot::OnFinish(const jsoncons::json& msg) {
  const auto& data = msg.get("data", jsoncons::json(""));
  const auto& color = data["color"].as_string();

  visualizer_.CarFinishedRace(color);

  bot_->CarFinishedRace(color);
  return ping();
}

RawBot::msg_vector RawBot::OnGameEnd(const jsoncons::json& msg) {
  visualizer_.GameEnd();
  std::cout << "Server: Game end" << std::endl;

  bot_->GameEnd(/* results */);
  return ping();
}

RawBot::msg_vector RawBot::OnTournamentEnd(const jsoncons::json& msg) {
  std::cout << "Server: Tournament end" << std::endl;
  bot_->TournamentEnd();
  return ping();
}

RawBot::msg_vector RawBot::OnCrash(const jsoncons::json& msg) {
  const auto& data = msg.get("data", jsoncons::json(""));
  const auto& color = data["color"].as_string();

  visualizer_.CarCrashed(color);
  bot_->CarCrashed(color);
  return ping();
}

RawBot::msg_vector RawBot::OnSpawn(const jsoncons::json& msg) {
  const auto& data = msg.get("data", jsoncons::json(""));
  const auto& color = data["color"].as_string();

  bot_->CarSpawned(color);
  return ping();
}

RawBot::msg_vector RawBot::OnError(const jsoncons::json& msg) {
  const auto& data = msg.get("data", jsoncons::json(""));
  std::cout << "Server: Error - " << data.to_string() << std::endl;
  return ping();
}

RawBot::msg_vector RawBot::OnDNF(const jsoncons::json& msg) {
  const auto& data = msg.get("data", jsoncons::json(""));
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

void RawBot::TransitionState(State from, State to) {
  if (state_ != from)
    std::cerr << "Incorrect state. state_=" << state_ << " from=" << from << " to=" << to << std::endl;

  state_ = to;
}

}  // namespace bots
