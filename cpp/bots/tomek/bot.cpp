#include "bots/tomek/bot.h"
#include <cstring>
#include <limits>

using game::Position;
using game::CarTracker;
using game::Race;
using game::Command;

namespace bots {
namespace tomek {

Bot::Bot() {
  srand(time(0));
}

Bot::~Bot() {
}

game::Command Bot::GetMove(
    const std::map<std::string, Position>& positions, int game_tick)  {
  const auto& my_position = positions.find(color_)->second;
  car_tracker_->Record(my_position);

  double throttle = 1;
  // throttle = 0.50 + double(rand() % 22)/99.0;
  // if (car_tracker_->angle() < 1) {
  //   throttle = 1.0;
  // } else if (car_tracker_->angle() < 30) {
  //   throttle = 0.6;
  // } else {
  //   throttle = 0.1;
  // }
  // if (positions.find(color_)->second.angle() > 10) {
  //   throttle = 0;
  //   count_++;
  // }

  // if (count_ > 0 && count_ < 5) {
  //   std::cout << game_tick << " " <<  std::setprecision(std::numeric_limits<long double>::digits10) << positions.find(color_)->second.angle() << std::endl;
  //   count_++;
  // }
  throttle = 0.50;

  if (my_position.piece() == 2) {
    if (my_position.start_lane() == 0) {
      return Command(game::kSwitchRight);
    } else {
      return Command(game::kSwitchLeft);
    }
  }


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

void Bot::OnTurbo(const game::Turbo& turbo) {
}

}  // namespace bots
}  // namespace tomek
