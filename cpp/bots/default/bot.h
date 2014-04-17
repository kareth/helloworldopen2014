#ifndef CPP_BOTS_DEFAULT_BOT_H_
#define CPP_BOTS_DEFAULT_BOT_H_

#include <string>
#include <map>
#include "bots/bot_interface.h"
#include "game/command.h"
#include "game/position.h"

namespace default_bot {

class Bot : public bots::BotInterface {
 public:
  Bot();

  void JoinedGame() override;
  void YourCar(const std::string& color) override;
  void NewRace(const game::Race& race) override;
  void GameStarted() override;
  game::Command GetMove(const std::map<std::string, game::Position>& positions) override;
  void CarFinishedLap(const std::string& color) override;
  void CarFinishedRace(const std::string& color) override;
  void GameEnd() override;
  void TournamentEnd() override;

  void CarCrashed(const std::string& color) override;
  void CarSpawned(const std::string& color) override;
};

}  // namespace default_bot

#endif  // CPP_BOTS_DEFAULT_BOT_H_
