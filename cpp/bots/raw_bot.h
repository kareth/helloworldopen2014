#ifndef CPP_BOTS_RAW_BOT_H_
#define CPP_BOTS_RAW_BOT_H_

#include <string>
#include <vector>
#include <map>
#include <functional>
#include <iostream>
#include "jsoncons/json.hpp"
#include "utils/protocol.h"

#include "bots/bot_interface.h"
#include "utils/game_visualizer.h"

namespace bots {

class RawBot {
 public:
  typedef std::vector<jsoncons::json> msg_vector;

  explicit RawBot(BotInterface* bot);
  ~RawBot();

  std::vector<jsoncons::json> React(const jsoncons::json& msg);

 private:
  typedef std::function<msg_vector(RawBot*, const jsoncons::json&)> action_fun;

  enum State {
    kWaitingForJoin = 0,
    kJoin = 1,
    kYourCar = 2,
    kGameInit = 3,
    kGameStart = 4,
  };

  msg_vector CommandToMsg(const game::Command& command, int game_tick);

  msg_vector OnJoin(const jsoncons::json& data);
  msg_vector OnYourCar(const jsoncons::json& data);
  msg_vector OnGameInit(const jsoncons::json& data);
  msg_vector OnGameStart(const jsoncons::json& data);
  msg_vector OnCarPositions(const jsoncons::json& data);
  msg_vector OnLapFinished(const jsoncons::json& data);
  msg_vector OnFinish(const jsoncons::json& data);
  msg_vector OnGameEnd(const jsoncons::json& data);
  msg_vector OnTournamentEnd(const jsoncons::json& data);

  msg_vector OnCrash(const jsoncons::json& data);
  msg_vector OnSpawn(const jsoncons::json& data);
  msg_vector OnError(const jsoncons::json& data);
  msg_vector OnDNF(const jsoncons::json& data);
  msg_vector OnTurboAvailable(const jsoncons::json& data);

  msg_vector ping() const { return { }; }

  // Prints color with color in terminal
  std::string ColorPrint(const std::string& color) const;

  std::unique_ptr<BotInterface> bot_;
  const std::map<std::string, action_fun> action_map_;
  utils::GameVisualizer visualizer_;

  void TransitionState(State from, State to);

  State state_ = State::kWaitingForJoin;
  int last_game_tick_ = -1;
  jsoncons::json history;
};

}  // namespace bots

#endif  // CPP_BOTS_RAW_BOT_H_
