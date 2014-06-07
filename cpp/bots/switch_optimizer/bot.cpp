#include "bots/switch_optimizer/bot.h"
#include "game/physics_params.h"
#include "utils/deadline.h"

using std::string;
using std::vector;
using std::map;

using game::CarTracker;
using game::CarState;
using game::Command;
using game::Position;
using game::Race;
using game::PhysicsParams;
using schedulers::Strategy;

namespace bots {
namespace switch_optimizer {

Bot::Bot() {
}

Bot::~Bot() {
}

void Bot::NewRace(const Race& race) {
  race_ = race;

  car_tracker_.reset(new CarTracker(&race_, PhysicsParams::Load()));

  throttle_scheduler_.reset(new schedulers::WojtekThrottleScheduler(race, *car_tracker_.get()));
}

game::Command Bot::GetMove(const map<string, Position>& positions, int game_tick)  {
  game_tick_ = game_tick;
  const Position& position = positions.at(color_);
  car_tracker_->Record(position);
  auto& state = car_tracker_->current_state();
  states_.push_back(state);

  //velocity_tracker_.Record(state);

  if (crashed_) return Command(0);

  utils::Deadline deadline(std::chrono::microseconds(3000));

  SetStrategy(state);
  throttle_scheduler_->Schedule(state, game_tick, deadline);
  Command command(throttle_scheduler_->throttle());

  car_tracker_->RecordCommand(command);
  return command;
}

void Bot::SetStrategy(const game::CarState& state) {
  throttle_scheduler_->set_strategy(Strategy::kOptimizeRace);
}

void Bot::ScheduleOvertakes() {
}

void Bot::OnTurbo(const game::Turbo& turbo) {
  if (!crashed_) {
    car_tracker_->RecordTurboAvailable(turbo);
  }
}

void Bot::YourCar(const string& color) {
  color_ = color;
}

void Bot::GameStarted() {
  started_ = true;
  car_tracker_->Reset();
  crashed_ = false;
}

void Bot::CarFinishedLap(const string& color, const game::Result& result)  {
}

void Bot::CarFinishedRace(const string& color)  {
}

void Bot::GameEnd(/* results */)  {
}

void Bot::TournamentEnd() {
}

void Bot::CarCrashed(const string& color)  {
  car_tracker_->spawn_model().RecordCrash(game_tick_);

  crashed_ = true;
  car_tracker_->RecordCarCrash();
}

void Bot::CarSpawned(const string& color)  {
  car_tracker_->spawn_model().RecordSpawn(game_tick_);
  if (color == color_) {
    crashed_ = false;
    car_tracker_->Reset();
  }
}

void Bot::CarDNF(const std::string& color) {
}

void Bot::TurboStarted(const std::string& color) {
}

void Bot::TurboEnded(const std::string& color) {
}

}  // namespace switch_optimizer
}  // namespace bots
