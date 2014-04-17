#ifndef CPP_BOTS_TOMEK_BOT_H_
#define CPP_BOTS_TOMEK_BOT_H_

#include <iostream>
#include <fstream>
#include <string>
#include <map>

#include "bots/bot_interface.h"
#include "game/command.h"
#include "game/position.h"
#include "game/race.h"

namespace bots {
namespace tomek {

class Bot : public bots::BotInterface {
 public:
  Bot();
  ~Bot() override;

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

 private:
  std::ofstream stats_file_;
  game::Race race_;
  std::string color_;

  double last_velocity_ = 0;
  vector<game::Position> positions_;
};

}  // namespace bots
}  // namespace tomek

#endif  // CPP_BOTS_TOMEK_BOT_H_
