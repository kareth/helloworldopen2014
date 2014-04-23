#ifndef CPP_BOTS_STEPPING_BOT_H_
#define CPP_BOTS_STEPPING_BOT_H_

#include <string>
#include <map>
#include "bots/bot_interface.h"
#include "game/command.h"
#include "game/position.h"

#include "game/race.h"
#include "game/piece.h"
#include "game/position.h"

#include "game/car_tracker.h"
#include "game/turbo.h"
#include "schedulers/greedy_turbo_scheduler.h"
//#include "schedulers/switch_scheduler.h"
#include "schedulers/binary_throttle_scheduler.h"

namespace bots {
namespace stepping {

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

  void OnTurbo(const game::Turbo& turbo) override;
  void TurboStarted(const std::string& color) override;
  void TurboEnded(const std::string& color) override;

 private:
  game::Race race_;

  std::string color_;
  bool started_ = false;
  bool crashed_ = false;

  std::unique_ptr<game::CarTracker> car_tracker_;

  int switched_ = -1;

  //SwitchScheduler switch_scheduler_;
  std::unique_ptr<schedulers::TurboScheduler> turbo_scheduler_;
  std::unique_ptr<schedulers::ThrottleScheduler> throttle_scheduler_;
};

}  // namespace stepping
}  // namespace bots

#endif  // CPP_BOTS_STEPPING_BOT_H_
