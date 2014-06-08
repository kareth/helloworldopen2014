#ifndef CPP_GAME_COMMAND_H_
#define CPP_GAME_COMMAND_H_

#include <sstream>

namespace game {

template <typename Enumeration>
auto as_integer(Enumeration const value)
    -> typename std::underlying_type<Enumeration>::type
{
    return static_cast<typename std::underlying_type<Enumeration>::type>(value);
}

enum class Switch {
  kSwitchLeft,
  kStay,
  kSwitchRight
};

enum class TurboToggle {
  kToggleOn
};

class Command {
 public:
  Command();
  explicit Command(double t);
  explicit Command(Switch s);
  explicit Command(TurboToggle t);

  Switch get_switch() const { return switch_; }
  double get_throttle() const { return throttle_; }
  double throttle() const { return throttle_; }

  void set_throttle(double t) { throttle_ = t; throttle_set_ = true; }
  void set_switch(Switch s) { switch_ = s; switch_set_ = true; }
  void set_turbo() { turbo_ = TurboToggle::kToggleOn; turbo_set_ = true; }
  //void set

  bool SwitchSet() const { return switch_set_; }
  bool ThrottleSet() const { return throttle_set_; }
  bool TurboSet() const { return turbo_set_; }

  static Command Turbo() { return Command(TurboToggle::kToggleOn); }

  std::string DebugString() const {
    std::stringstream ss;
    if (switch_set_) {
      ss << "switch: " << as_integer(switch_) << std::endl;
    } else if (throttle_set_) {
      ss << "throttle: " << throttle_ << std::endl;
    } else if (turbo_set_) {
      ss << "turbo: " << as_integer(turbo_) << std::endl;
    } else {
      ss << "ping()" << std::endl;
    }
    return ss.str();
  }

  bool operator==(const Command& c) const {
    return switch_set_ == c.switch_set_ &&
           throttle_set_ == c.throttle_set_ &&
           turbo_set_ == c.turbo_set_ &&
           throttle_ == c.throttle_ &&
           switch_ == c.switch_ &&
           turbo_ == c.turbo_;
  }

 private:
  bool switch_set_ = false;
  bool throttle_set_ = false;
  bool turbo_set_ = false;

  double throttle_ = 0.0;
  Switch switch_ = Switch::kStay;
  TurboToggle turbo_;
};

}  // namespace game

#endif  // CPP_GAME_COMMAND_H_
