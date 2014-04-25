#ifndef CPP_UTILS_PROTOCOL_H_
#define CPP_UTILS_PROTOCOL_H_

#include <string>
#include <iostream>
#include "jsoncons/json.hpp"

#include "game/command.h"

namespace utils {

jsoncons::json make_request(const std::string& msg_type, const jsoncons::json& data);
jsoncons::json make_join(const std::string& name, const std::string& key);
jsoncons::json make_join_race(const std::string& name, const std::string& key,
                              const std::string& track, int players);
jsoncons::json make_ping();
jsoncons::json make_switch(game::Switch s, int game_tick=-1);
jsoncons::json make_throttle(double throttle, int game_tick=-1);
jsoncons::json make_turbo(int game_tick=-1);


}  // namespace utils

#endif  // CPP_UTILS_PROTOCOL_H_
