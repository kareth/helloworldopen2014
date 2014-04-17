#ifndef CPP_BOTS_DEFAULT_BOT_H_
#define CPP_BOTS_DEFAULT_BOT_H_

#include "bots/bot_interface.h"
#include "game/command.h"

namespace default_bot {

class Bot : public bots::BotInterface {
 public:
  Bot();

  void JoinedGame();
  void YourCar();
  void NewRace();
  void GameStarted();
  game::Command GetMove();
  void CarFinishedLap();
  void CarFinishedRace();
  void GameEnd();
  void TournamentEnd();

  void CarCrashed();
  void CarSpawned();
};

}  // namespace default_bot

#endif  // CPP_BOTS_DEFAULT_BOT_H_
