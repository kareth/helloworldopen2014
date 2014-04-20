#include "bots/kareth/bot.h"

using std::string;
using std::vector;
using std::map;

using game::Position;
using game::CarTracker;
using game::Race;
using game::Command;

namespace bots {
namespace kareth {

Bot::Bot() {
}

game::Command Bot::GetMove(const map<string, Position>& positions)  {
  Position position = positions.at(color_);
  Position previous;
  if (car_tracker_->positions().size() == 0)
    previous = position;
  else
    previous = car_tracker_->positions().back();

  car_tracker_->Record(position);

  double throttle = 0.65;

  auto predicted = car_tracker_->Predict(position, previous, throttle, 0);

  car_tracker_->RecordThrottle(throttle);
  return Command(throttle);
}

void Bot::JoinedGame() {
}

void Bot::YourCar(const string& color) {
  color_ = color;
}

void Bot::NewRace(const Race& race) {
  race_ = race;
  car_tracker_.reset(new CarTracker(&race_));
}

void Bot::GameStarted() {
}

void Bot::CarFinishedLap(const string& color /* + results */)  {
}

void Bot::CarFinishedRace(const string& color)  {
}

void Bot::GameEnd(/* results */)  {
}

void Bot::TournamentEnd()  {
}

void Bot::CarCrashed(const string& color)  {
  car_tracker_->RecordCarCrash();
}

void Bot::CarSpawned(const string& color)  {
}

}  // namespace kareth
}  // namespace bots
