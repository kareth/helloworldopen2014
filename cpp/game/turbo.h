#ifndef CPP_GAME_TURBO_H_
#define CPP_GAME_TURBO_H_

#include <string>
#include <iostream>
#include "jsoncons/json.hpp"

namespace game {

class Turbo {
 public:
  void ParseFromJson(const jsoncons::json& data);

  int duration() const { return ticks_; }
  int factor() const { return factor_; }

 private:
  double factor_;
  int ticks_;
};

}  // namespace game

#endif  // CPP_GAME_TURBO_H_
