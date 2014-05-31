#include "bots/constant/bot.h"

#include <cstring>
#include <limits>

#include "gflags/gflags.h"

using game::Position;
using game::CarTracker;
using game::Race;
using game::Command;
using game::Switch;

DEFINE_double(constant_throttle, 0.5, "Throttle to use in constant bot");
DECLARE_bool(always_switch);


namespace bots {
namespace constant {

Bot::Bot() {
  srand(time(0));
}

Bot::~Bot() {
}

game::Command Bot::ComputeMove(const Position& position, int game_tick) {
  if (FLAGS_always_switch) {
    int r = rand() % 100;
    if (r == 0) return Command(game::Switch::kSwitchLeft);
    if (r == 1) return Command(game::Switch::kSwitchRight);
  }
  return Command(FLAGS_constant_throttle);
}

game::Command Bot::GetMove(
    const std::map<std::string, Position>& positions, int game_tick)  {
  const auto& my_position = positions.find(color_)->second;
  car_tracker_->Record(my_position);

  Command command = ComputeMove(my_position, game_tick);

  car_tracker_->RecordCommand(command);
  return Command(command);
}

void Bot::GameStarted() {
  car_tracker_->Reset();
}

void Bot::YourCar(const std::string& color) {
  color_ = color;
}

void Bot::NewRace(const Race& race) {
  race_ = race;
  car_tracker_.reset(new CarTracker(&race_));
}

void Bot::CarCrashed(const std::string& color)  {
  crash_ = true;
  car_tracker_->RecordCarCrash();
}

void Bot::OnTurbo(const game::Turbo& turbo) {
  turbo_ = true;
  car_tracker_->RecordTurboAvailable(turbo);
}

}  // namespace bots
}  // namespace constant
