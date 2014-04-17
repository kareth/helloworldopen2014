#ifndef CPP_BOTS_DEFAULT_BOT_H_
#define CPP_BOTS_DEFAULT_BOT_H_

#include <string>
#include "bots/bot_interface.h"
#include "game/command.h"

namespace default_bot {

class Bot : public bots::BotInterface {
 public:
  Bot();

  void JoinedGame();
  void YourCar(const std::string& color);
  void NewRace();
  void GameStarted();
  game::Command GetMove();
  void CarFinishedLap();
  void CarFinishedRace(const std::string& color);
  void GameEnd();
  void TournamentEnd();

  void CarCrashed(const std::string& color);
  void CarSpawned(const std::string& color);
};

}  // namespace default_bot

#endif  // CPP_BOTS_DEFAULT_BOT_H_
