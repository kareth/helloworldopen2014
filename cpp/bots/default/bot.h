#ifndef CPP_BOTS_DEFAULT_BOT_H_
#define CPP_BOTS_DEFAULT_BOT_H_

#include "bots/bot_interface.h"
#include "game/command.h"

namespace default_bot {

class Bot : public bots::BotInterface {
 public:
  Bot();

  game::Command OnJoin();
  game::Command OnYourCar();
  game::Command OnGameInit();
  game::Command OnGameStart();
  game::Command OnCarPositions();
  game::Command OnLapFinished();
  game::Command OnFinish();
  game::Command OnGameEnd();

  game::Command OnCrash();
  game::Command OnSpawn();
  game::Command OnError();
};

}  // namespace default_bot

#endif  // CPP_BOTS_DEFAULT_BOT_H_
