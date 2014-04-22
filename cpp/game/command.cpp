#include "game/command.h"

namespace game {

Command::Command()
  : throttle_set_(false),
    switch_set_(false),
    turbo_set_(false) {
}

Command::Command(double t)
    : throttle_set_(true),
      switch_set_(false),
      turbo_set_(false),
      throttle_(t) {
}

Command::Command(TurboToggle t)
    : turbo_set_(true),
      throttle_set_(false),
      switch_set_(false),
      turbo_(t) {
}

Command::Command(Switch s)
    : switch_set_(true),
      turbo_set_(false),
      throttle_set_(false),
      switch_(s) {
}

}  // namespace game
