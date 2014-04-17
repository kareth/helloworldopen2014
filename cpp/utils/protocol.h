#ifndef CPP_UTILS_PROTOCOL_H_
#define CPP_UTILS_PROTOCOL_H_

#include <string>
#include <iostream>
#include "jsoncons/json.hpp"

#include "game/command.h"

namespace utils {

jsoncons::json make_request(const std::string& msg_type, const jsoncons::json& data);
jsoncons::json make_join(const std::string& name, const std::string& key);
jsoncons::json make_ping();
jsoncons::json make_switch(game::Switch s);
jsoncons::json make_throttle(double throttle);

}  // namespace utils

#endif  // CPP_UTILS_PROTOCOL_H_
