#include "bots/stepping/bot.h"
#include <cstring>

using std::string;
using std::vector;
using std::map;

using game::CarTracker;
using game::CarState;
using game::Command;
using game::Position;
using game::Race;

namespace bots {
namespace stepping {

Bot::Bot() {
  srand(time(0));
}

game::Command Bot::GetMove(const map<string, Position>& positions, int game_tick)  {
  const Position& position = positions.at(color_);
  car_tracker_->Record(position);
  auto& state = car_tracker_->current_state();

  throttle_scheduler_->Schedule(state);
  auto command = Command(throttle_scheduler_->throttle());

  car_tracker_->RecordCommand(command);
  return command;

  /*
  if (crashed_) {
    return Command(0);
  }

  //double throttle = Optimize();
  double throttle = 0.6;
  if (game_tick < 10) throttle = 1;
*/
  /*
  //if (CanUseTurbo(position)) {
  if (turbo_available_ && position.piece() == 28 && position.piece_distance() > 10) {
    printf("\n\n\n\n\nYABADABADUUUUU :D\n\n\n\n\n");
    turbo_on_ = turbo_.duration() - 1;
    turbo_available_ = false;
    car_tracker_->RecordCommand(Command(throttle*3.0));
    return Command(game::TurboToggle::kToggleOn);
  }

  if (race_.track().pieces()[car_tracker_->Predict(car_tracker_->current_state(), Command(1)).position().piece()].has_switch()) {
    if (position.start_lane() == 0)
      return Command(game::Switch::kSwitchRight);
    else
      return Command(game::Switch::kSwitchLeft);
  }
*/

  /*game::Switch s;
  if (ShouldChangeLane(position, &s)) {
    printf("Switch!\n");
    return Command(s);
  }*/
/*
  if (turbo_on_ > 0) {
    // We dont want to spoil model before its done
    turbo_on_--;
    return Command(1);
  }

  if (position.lap() == 2 && race_.track().IsLastStraight(position))
    return Command(1);

  car_tracker_->RecordCommand(Command(throttle));
  return Command(throttle);
*/
}



bool Bot::CanUseTurbo(const Position& position) {
  if (turbo_on_ > 0 || turbo_available_ == false)
    return false;

  // TODO hardcode
  //if (race_.track().id() == "keimola" && position.piece() == 36)
  //  return true;
  if (race_.track().IsLastStraight(position))
    return true;

  return false;
}

void Bot::JoinedGame() {
}

void Bot::YourCar(const string& color) {
  color_ = color;
}

// GameInit
void Bot::NewRace(const Race& race) {
  race_ = race;
  car_tracker_.reset(new CarTracker(&race_));
  throttle_scheduler_.reset(
      new schedulers::BinaryThrottleScheduler(race_, *car_tracker_.get(), 300));
}

void Bot::GameStarted() {
  started_ = true;
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
  auto& state = car_tracker_->current_state();
  auto next = car_tracker_->Predict(state, Command(car_tracker_->throttle()));
  printf("Crash! %lf %lf %lf\n", next.position().angle(), state.position().angle(), state.previous_angle());

  if (color == color_) {
    crashed_ = true;
    car_tracker_->RecordCarCrash();
  }
}

void Bot::CarSpawned(const string& color)  {
  if (color == color_)
    crashed_ = false;
}

void Bot::OnTurbo(const game::Turbo& turbo) {
  //turbo_scheduler_.Schedule(turbo, );
  printf("Turbo Available\n");
  turbo_available_ = true;
  turbo_ = turbo;
}

void Bot::TurboStarted(const std::string& color) {
}

void Bot::TurboEnded(const std::string& color) {
}

}  // namespace stepping
}  // namespace bots
