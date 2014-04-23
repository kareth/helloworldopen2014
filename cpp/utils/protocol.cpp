#include "utils/protocol.h"

namespace utils {

jsoncons::json make_request(const std::string& msg_type, const jsoncons::json& data) {
  jsoncons::json r;
  r["msgType"] = msg_type;
  r["data"] = data;
  return r;
}

jsoncons::json make_join(const std::string& name, const std::string& key) {
  jsoncons::json data;
  data["name"] = name;
  data["key"] = key;
  return make_request("join", data);
}

jsoncons::json make_join_race(const std::string& name, const std::string& key,
                              const std::string& track, int players) {
  jsoncons::json bot;
  bot["name"] = name;
  bot["key"] = key;

  jsoncons::json data;
  data["trackName"] = track;
  data["carCount"] = players;
  data["botId"] = bot;

  return make_request("joinRace", data);
}

jsoncons::json make_ping() {
  return make_request("ping", jsoncons::null_type());
}

jsoncons::json make_throttle(double throttle, int game_tick) {
  jsoncons::json r;
  r["msgType"] = "throttle";
  r["data"] = throttle;
  if (game_tick != -1) {
    r["gameTick"] = game_tick;
  }
  return r;
}

jsoncons::json make_switch(game::Switch s) {
  if (s == game::Switch::kSwitchLeft)
    return make_request("switchLane", "Left");
  else if (s == game::Switch::kSwitchRight)
    return make_request("switchLane", "Right");
  else
    return make_ping();
}

jsoncons::json make_turbo() {
  return make_request("turbo", "YABAA DABAA DUUUUUUUUUUUU");
}

}  // namespace utils
