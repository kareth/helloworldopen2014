#ifndef CPP_BOTS_SWITCH_OPTIMIZER_BOT_H_
#define CPP_BOTS_SWITCH_OPTIMIZER_BOT_H_

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

#include "gflags/gflags.h"

#include "schedulers/wojtek_throttle_scheduler.h"

#include "game/velocity_predictor.h"

namespace bots {
namespace switch_optimizer {

class Bot : public bots::BotInterface {
 public:
  Bot();
  explicit Bot(game::VelocityPredictor* velocity_predictor);
  ~Bot();

  void JoinedGame() override {}
  void YourCar(const std::string& color) override;
  void NewRace(const game::Race& race) override;
  void GameStarted() override;
  game::Command GetMove(const std::map<std::string, game::Position>& positions, int game_tick) override;
  void CarFinishedLap(const std::string& color, const game::Result& result) override;
  void CarFinishedRace(const std::string& color) override;
  void GameEnd() override;
  void TournamentEnd() override;

  void CarCrashed(const std::string& color) override;
  void CarSpawned(const std::string& color) override;

  void OnTurbo(const game::Turbo& turbo) override;
  void TurboStarted(const std::string& color) override;
  void TurboEnded(const std::string& color) override;

  void CarDNF(const std::string& color) override;

  const std::vector<game::CarState>& states() { return states_; }

 private:
  void SetStrategy(const game::CarState& state);
  void ScheduleOvertakes();

  game::Race race_;
  std::vector<game::CarState> states_;

  game::VelocityPredictor* velocity_predictor_;
  std::unique_ptr<game::VelocityPredictor> velocity_predictor_uniq_;

  std::string color_;
  bool started_ = false;
  bool crashed_ = false;
  int game_tick_;

  std::unique_ptr<game::CarTracker> car_tracker_;

  std::unique_ptr<schedulers::ThrottleScheduler> throttle_scheduler_;
};

}  // namespace switch_optimizer
}  // namespace bots

#endif  // CPP_BOTS_SWITCH_OPTIMIZER_BOT_H_
