#ifndef HWO_BOT_API_H_
#define HWO_BOT_API_H_

#include <string>
#include <vector>
#include <map>
#include <functional>
#include <iostream>
#include <jsoncons/json.hpp>

namespace bots {

class BotAPI {
 public:
  virtual std::vector<jsoncons::json> react(const jsoncons::json& msg) = 0;
};

}  // namespace bots

#endif  // BOTS_BOT_API_H_
