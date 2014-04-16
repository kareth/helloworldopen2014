#ifndef CPP_BOTS_DEFAULT_BOT_H_
#define CPP_BOTS_DEFAULT_BOT_H_

#include <string>
#include <vector>
#include <map>
#include <functional>
#include <iostream>

#include "jsoncons/json.hpp"

#include "utils/protocol.h"
#include "bots/bot_api.h"

namespace default_bot {

class Bot : public bots::BotAPI {
 public:
  typedef std::vector<jsoncons::json> msg_vector;

  Bot();
  msg_vector react(const jsoncons::json& msg);

 private:
  typedef std::function<msg_vector(Bot*, const jsoncons::json&)> action_fun;
  const std::map<std::string, action_fun> action_map;

  msg_vector on_join(const jsoncons::json& data);
  msg_vector on_game_start(const jsoncons::json& data);
  msg_vector on_car_positions(const jsoncons::json& data);
  msg_vector on_crash(const jsoncons::json& data);
  msg_vector on_game_end(const jsoncons::json& data);
  msg_vector on_error(const jsoncons::json& data);
};

}  // namespace default_bot

#endif  // CPP_BOTS_DEFAULT_BOT_H_
