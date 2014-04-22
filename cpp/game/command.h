#ifndef CPP_GAME_COMMAND_H_
#define CPP_GAME_COMMAND_H_

namespace game {

enum Switch {
  kSwitchLeft,
  kStay,
  kSwitchRight
};

enum class Turbo {
  kOn
};

class Command {
 public:
  Command();
  explicit Command(double t);
  explicit Command(Switch s);
  explicit Command(Turbo t);

  Switch get_switch() const { return switch_; }
  double get_throttle() const { return throttle_; }
  Turbo get_turbo() const { return turbo_; }

  void set_throttle(double t) { throttle_ = t; throttle_set_ = true; }
  void set_switch(Switch s) { switch_ = s; switch_set_ = true; }
  void set_turbo(Turbo t) { turbo_ = t; turbo_set_ = true; }
  //void set

  bool SwitchSet() const { return switch_set_; }
  bool ThrottleSet() const { return throttle_set_; }
  bool TurboSet() const { return turbo_set_; }

 private:
  bool switch_set_;
  bool throttle_set_;
  bool turbo_set_;

  double throttle_;
  Switch switch_;
  Turbo turbo_;
};

}  // namespace game

#endif  // CPP_GAME_COMMAND_H_
