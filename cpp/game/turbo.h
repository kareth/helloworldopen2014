#ifndef CPP_GAME_TURBO_H_
#define CPP_GAME_TURBO_H_

#include <string>
#include <iostream>
#include <sstream>

#include "jsoncons/json.hpp"

namespace game {

class Turbo {
 public:
  Turbo() {}
  Turbo(int duration, double factor) : ticks_(duration), factor_(factor) {}
  void ParseFromJson(const jsoncons::json& data);

  // Number of ticks turbo can be on.
  int duration() const { return ticks_; }
  double factor() const { return factor_; }

  std::string DebugString() const {
    std::stringstream ss;
    ss << "duration: " << ticks_ << std::endl;
    ss << "factor: " << factor_ << std::endl;
    return ss.str();
  }

 private:
  double factor_ = 1.0;
  int ticks_ = 0;
};

}  // namespace game

#endif  // CPP_GAME_TURBO_H_
