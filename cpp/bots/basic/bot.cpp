#include "bots/basic/bot.h"

namespace bots {
namespace basic {

Bot::Bot() {
}

void Bot::JoinedGame() {
  return;
}

void Bot::YourCar(const std::string& color) {
  return;
}

void Bot::NewRace(const game::Race& race) {
  return;
}

void Bot::GameStarted() {
  return;
}

game::Command Bot::GetMove(const std::map<std::string, game::Position>& positions, int game_tick) {
  return game::Command(0.65);
}

void Bot::CarFinishedLap(const std::string& color /* + results */)  {
  return;
}

void Bot::CarFinishedRace(const std::string& color)  {
  return;
}

void Bot::GameEnd(/* results */)  {
  return;
}

void Bot::TournamentEnd()  {
  return;
}

void Bot::CarCrashed(const std::string& color)  {
  return;
}

void Bot::CarSpawned(const std::string& color)  {
  return;
}

}  // namespace basic
}  // namespace bots
