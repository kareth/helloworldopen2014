#ifndef CPP_GAME_COMMAND_H_
#define CPP_GAME_COMMAND_H_

namespace game {

enum Switch {
  kSwitchLeft,
  kStay,
  kSwitchRight
};

class Command {
 public:
  Command();
  explicit Command(double t);
  // TODO(tomek) Can you have both throttle and switch????
  Command(double t, Switch s);

  Switch get_switch() const { return switch_; }
  double get_throttle() const { return throttle_; }

  void set_throttle(double t) { throttle_ = t; throttle_set_ = true; }
  void set_switch(Switch s) { switch_ = s; switch_set_ = true; }

  bool SwitchSet() const { return switch_set_; }
  bool ThrottleSet() const { return throttle_set_; }

 private:
  bool switch_set_;
  bool throttle_set_;

  double throttle_;
  Switch switch_;
};

}  // namespace game

#endif  // CPP_GAME_COMMAND_H_
