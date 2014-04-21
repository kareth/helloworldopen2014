#ifndef CPP_BOTS_GREEDY_BOT_H_
#define CPP_BOTS_GREEDY_BOT_H_

#include <string>
#include <map>
#include "bots/bot_interface.h"
#include "game/command.h"
#include "game/position.h"

#include "game/race.h"
#include "game/piece.h"
#include "game/position.h"

#include "game/car_tracker.h"

namespace bots {
namespace greedy {

class Bot : public bots::BotInterface {
 public:
  Bot();

  void JoinedGame() override;
  void YourCar(const std::string& color) override;
  void NewRace(const game::Race& race) override;
  void GameStarted() override;
  game::Command GetMove(const std::map<std::string, game::Position>& positions, int game_tick) override;
  void CarFinishedLap(const std::string& color) override;
  void CarFinishedRace(const std::string& color) override;
  void GameEnd() override;
  void TournamentEnd() override;

  void CarCrashed(const std::string& color) override;
  void CarSpawned(const std::string& color) override;

 private:
  double Optimize(const game::Position& previous, const game::Position& current);

  game::Race race_;

  std::string color_;
  bool started_ = false;
  bool crashed_ = false;

  std::unique_ptr<game::CarTracker> car_tracker_;
};

}  // namespace greedy
}  // namespace bots

#endif  // CPP_BOTS_GREEDY_BOT_H_
