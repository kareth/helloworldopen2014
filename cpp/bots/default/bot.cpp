#include "bots/default/bot.h"

namespace default_bot {

Bot::Bot() {
}

void Bot::JoinedGame() {
  return;
}

void Bot::YourCar(const std::string& color) {
  return;
}

void Bot::NewRace(/* Race */) {
  return;
}

void Bot::GameStarted() {
  return;
}

game::Command Bot::GetMove(/* vektor/mapa pozycji aut */)  {
  return game::Command(0.65);
}

void Bot::CarFinishedLap(/* Car and results */)  {
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

}  // namespace default_bot
