#include "bots/tomek/bot.h"

using game::Position;
using game::CarTracker;
using game::Race;
using game::Command;

namespace bots {
namespace tomek {

Bot::Bot() {
}

Bot::~Bot() {
}

game::Command Bot::GetMove(
    const std::map<std::string, Position>& positions, int game_tick)  {
  car_tracker_->Record(positions.find(color_)->second);

  double throttle = 0.65;

  car_tracker_->RecordThrottle(throttle);
  return Command(throttle);
}

void Bot::JoinedGame() {
}

void Bot::YourCar(const std::string& color) {
  color_ = color;
}

void Bot::NewRace(const Race& race) {
  race_ = race;
  car_tracker_.reset(new CarTracker(&race_));
}

void Bot::GameStarted() {
}

void Bot::CarFinishedLap(const std::string& color /* + results */)  {
}

void Bot::CarFinishedRace(const std::string& color)  {
}

void Bot::GameEnd(/* results */)  {
}

void Bot::TournamentEnd()  {
}

void Bot::CarCrashed(const std::string& color)  {
  car_tracker_->RecordCarCrash();
}

void Bot::CarSpawned(const std::string& color)  {
}

}  // namespace bots
}  // namespace tomek
