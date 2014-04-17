#include "game/command.h"

namespace game {

Command::Command()
  : throttle_set_(false), switch_set_(false) {
}

Command::Command(double t)
    : throttle_set_(true), switch_set_(false), throttle_(t) {
}

Command::Command(double t, Switch s)
    : throttle_set_(true), switch_set_(true), throttle_(t), switch_(s) {
}

}  // namespace command
