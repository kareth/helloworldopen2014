#include "bots/default/bot.h"

namespace default_bot {

Bot::Bot() {
}

game::Command Bot::OnJoin() {
  return game::Command();
}

game::Command Bot::OnYourCar() {
  return game::Command();
}

game::Command Bot::OnGameInit() {
  return game::Command();
}

game::Command Bot::OnGameStart() {
  return game::Command();
}
game::Command Bot::OnCarPositions()  {
  return game::Command(0.65);
}

game::Command Bot::OnLapFinished()  {
  return game::Command();
}

game::Command Bot::OnFinish()  {
  return game::Command();
}

game::Command Bot::OnGameEnd()  {
  return game::Command();
}

game::Command Bot::OnCrash()  {
  return game::Command();
}

game::Command Bot::OnSpawn()  {
  return game::Command();
}

game::Command Bot::OnError()  {
  return game::Command();
}

}  // namespace default_bot
