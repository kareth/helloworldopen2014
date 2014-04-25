#ifndef CPP_BOTS_PIOTR_BOT_H_
#define CPP_BOTS_PIOTR_BOT_H_

#include <string>
#include <map>
#include "bots/bot_interface.h"
#include "game/command.h"
#include "game/position.h"

#include "game/race.h"
#include "game/piece.h"

#include "physics/speed_tracker.h"
#include "game/turbo.h"

namespace bots {
namespace piotr {

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

  void OnTurbo(const game::Turbo& turbo);

 private:
  double DistanceFromBent(const game::Position& position, double* angle) const;

  game::Race race_;
  std::unique_ptr<physics::SpeedTracker> speed_tracker_;
  std::string color_;
};

}  // namespace piotr
}  // namespace bots

#endif  // CPP_BOTS_PIOTR_BOT_H_
