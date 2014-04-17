#ifndef CPP_BOTS_BOT_INTERFACE_H_
#define CPP_BOTS_BOT_INTERFACE_H_

#include "game/command.h"

namespace bots {

class BotInterface {
 public:
  virtual game::Command OnJoin() = 0;
  virtual game::Command OnYourCar() = 0;
  virtual game::Command OnGameInit() = 0;
  virtual game::Command OnGameStart() = 0;
  virtual game::Command OnCarPositions() = 0;
  virtual game::Command OnGameEnd() = 0;

  virtual game::Command OnCrash() = 0;
  virtual game::Command OnSpawn() = 0;
  virtual game::Command OnLapFinished() = 0;
  virtual game::Command OnFinish() = 0;
  virtual game::Command OnError() = 0;
};

}  // namespace bots

#endif  // CPP_BOTS_BOT_INTERFACE_H_
