#ifndef CPP_BOTS_BOT_INTERFACE_H_
#define CPP_BOTS_BOT_INTERFACE_H_

#include <string>
#include "game/command.h"

namespace bots {

class BotInterface {
 public:
  virtual void JoinedGame() = 0;
  virtual void YourCar(const std::string& color) = 0;
  virtual void NewRace() = 0;
  virtual void GameStarted() = 0;
  virtual game::Command GetMove() = 0;
  virtual void CarFinishedLap() = 0;
  virtual void CarFinishedRace(const std::string& color) = 0;
  virtual void GameEnd() = 0;
  virtual void TournamentEnd() = 0;

  virtual void CarCrashed(const std::string& color) = 0;
  virtual void CarSpawned(const std::string& color) = 0;
};

}  // namespace bots

#endif  // CPP_BOTS_BOT_INTERFACE_H_
