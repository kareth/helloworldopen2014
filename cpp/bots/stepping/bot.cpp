#include "bots/stepping/bot.h"

DEFINE_int32(answer_time, 10, "Time limit for answer in ms");

using std::string;
using std::vector;
using std::map;

using game::CarTracker;
using game::CarState;
using game::Command;
using game::Position;
using game::Race;
using schedulers::Strategy;

namespace bots {
namespace stepping {

Bot::Bot() {
}

void Bot::NewRace(const Race& race) {
  race_ = race;
  // We do not want to loose all models between qualification and race.
  // TODO We assume that the race is exactly the same as the one in car_tracker_.
  if (car_tracker_ == nullptr) {
    car_tracker_.reset(new CarTracker(&race_));
  }

  scheduler_.reset(
      new schedulers::BulkScheduler(
        race_, *car_tracker_.get(), FLAGS_answer_time));

  learning_scheduler_.reset(
      new schedulers::LearningScheduler(
        race_, *car_tracker_.get(), FLAGS_answer_time));
}

game::Command Bot::GetMove(const map<string, Position>& positions, int game_tick)  {
  const Position& position = positions.at(color_);
  car_tracker_->Record(position);
  auto& state = car_tracker_->current_state();

  // TODO
  if (crashed_)
    return Command(0);
  if (race_.track().id() == "usa" &&
      position.lap() == 0) {
    auto command = Command(0.4);
    car_tracker_->RecordCommand(command);
    return command;
  }

  Command command;
  if (car_tracker_->IsReady()) {
    SetStrategy(state);
    scheduler_->Schedule(state);
    command = scheduler_->command();
    scheduler_->IssuedCommand(command);
  } else {
    learning_scheduler_->Schedule(state);
    command = learning_scheduler_->command();
    learning_scheduler_->IssuedCommand(command);
  }

  car_tracker_->RecordCommand(command);
  return command;
}

void Bot::SetStrategy(const game::CarState& state) {
  if (race_.laps() == -1) {
    scheduler_->set_strategy(Strategy::kOptimizeRace);
    return;
  }

  int lap = state.position().lap();

  if (lap % 2 == 1)
    scheduler_->set_strategy(Strategy::kOptimizeNextLap);
  else if (lap % 2 == 0 && lap != 0)
    scheduler_->set_strategy(Strategy::kOptimizeCurrentLap);
  else
    scheduler_->set_strategy(Strategy::kOptimizeRace);
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
  car_tracker_->Reset();
}


void Bot::TurboStarted(const std::string& color) {
}

void Bot::TurboEnded(const std::string& color) {
}

}  // namespace stepping
}  // namespace bots
