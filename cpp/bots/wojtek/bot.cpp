#include "bots/wojtek/bot.h"
#include "schedulers/wojtek_throttle_scheduler.h"

DECLARE_int32(handicap);
DECLARE_bool(check_if_safe_ahead);
DECLARE_int32(answer_time);
DECLARE_bool(bump_with_turbo);
DECLARE_bool(defend_turbo_bump);

using std::string;
using std::vector;
using std::map;

using game::CarTracker;
using game::RaceTracker;
using game::CarState;
using game::Command;
using game::Position;
using game::Race;
using schedulers::Strategy;

namespace bots {
namespace wojtek {

Bot::Bot() {
}

void Bot::NewRace(const Race& race) {
  race_ = race;
  // We do not want to loose all models between qualification and race.
  // TODO We assume that the race is exactly the same as the one in car_tracker_.
  // (kareth) Is resetting race enough? it only differs in laps/duration
  if (car_tracker_ == nullptr) {
    car_tracker_.reset(new CarTracker(&race_));
  } else {
    car_tracker_->set_race(&race_);
  }

  throttle_scheduler_.reset(new schedulers::WojtekThrottleScheduler(&race_, car_tracker_.get()));
}

game::Command Bot::GetMove(const map<string, Position>& positions, int game_tick)  {
  const Position& position = positions.at(color_);
  car_tracker_->Record(position, car_tracker_->HasSomeoneMaybeBumpedMe(positions, color_));
  auto& state = car_tracker_->current_state();

  throttle_scheduler_->Schedule(state);
  Command command = Command(throttle_scheduler_->throttle());

  car_tracker_->RecordCommand(command);
  return command;
}

void Bot::OnTurbo(const game::Turbo& turbo) {
  if (!crashed_) {
    car_tracker_->RecordTurboAvailable(turbo);
    printf("Turbo Available\n");
  }
}

void Bot::YourCar(const string& color) {
  color_ = color;
}

void Bot::GameStarted() {
  started_ = true;
  car_tracker_->Reset();
}

void Bot::CarFinishedLap(const string& color, const game::Result& result)  {
}

void Bot::CarFinishedRace(const string& color)  {
}

void Bot::GameEnd(/* results */)  {
}

void Bot::TournamentEnd()  {
}

void Bot::CarCrashed(const string& color)  {
  auto& state = car_tracker_->current_state();
  auto next = car_tracker_->Predict(state, Command(car_tracker_->throttle()));
  printf("Crash! %lf %lf %lf %s\n", next.position().angle(), state.position().angle(), state.previous_angle(), color.c_str());

  if (color == color_) {
    crashed_ = true;
    car_tracker_->RecordCarCrash();
  }
}

void Bot::CarSpawned(const string& color)  {
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

}  // namespace wojtek
}  // namespace bots
