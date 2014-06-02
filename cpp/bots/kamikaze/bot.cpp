#include "bots/kamikaze/bot.h"
#include "game/physics_params.h"

DECLARE_int32(handicap);
DECLARE_int32(answer_time);

using std::string;
using std::vector;
using std::map;

using game::CarTracker;
using game::RaceTracker;
using game::CarState;
using game::Command;
using game::Position;
using game::Race;
using game::PhysicsParams;
using schedulers::Strategy;

namespace bots {
namespace kamikaze {

Bot::Bot() {
}

void Bot::NewRace(const Race& race) {
  race_ = race;
  // We do not want to loose all models between qualification and race.
  // TODO We assume that the race is exactly the same as the one in car_tracker_.
  // (kareth) Is resetting race enough? it only differs in laps/duration
  if (car_tracker_ == nullptr) {
    car_tracker_.reset(new CarTracker(&race_, PhysicsParams::Load()));
    race_tracker_.reset(new RaceTracker(*car_tracker_.get(), race_, color_));
    bump_tracker_.reset(new game::BumpTracker(*car_tracker_.get(), race_));
  } else {
    car_tracker_->set_race(&race_);
  }

  scheduler_.reset(
      new schedulers::BulkScheduler(
        race_, *race_tracker_.get(), *car_tracker_.get(), FLAGS_answer_time));
}

std::map<string, CarState> tmp_states;

game::Command Bot::GetMove(const map<string, Position>& positions, int game_tick)  {
  const Position& position = positions.at(color_);
  car_tracker_->Record(position);
  auto& state = car_tracker_->current_state();

  if (game_tick < FLAGS_handicap) {
    return Command(0);
  }

  //race_tracker_->Record(positions);
  race_tracker_->Record(positions);

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
  SetStrategy(state);

  scheduler_->Schedule(state);
  command = scheduler_->command();

  for (auto& p : positions) {
    if (p.first != color_) {
      auto& enemy = race_tracker_->enemy(p.first).state();
      if (!race_tracker_->enemy(p.first).is_dead() &&
          car_tracker_->IsSafe(enemy) &&
          bump_tracker_->CanBumpWithTurbo(state, enemy) &&
          state.position().end_lane() == enemy.position().end_lane()) {
            if (state.turbo_state().is_on() ||
                !state.turbo_state().available())
              command = Command(1);
            else
              command = Command::Turbo();
      }
    }
  }

  scheduler_->IssuedCommand(command);

  car_tracker_->RecordCommand(command);
  return command;
}

void Bot::SetStrategy(const game::CarState& state) {
  if (race_.laps() != -1) {
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

void Bot::CarFinishedLap(const string& color, const game::Result& result)  {
  race_tracker_->RecordLapTime(color, result.lap_time());
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

  race_tracker_->RecordCrash(color);
}

void Bot::CarSpawned(const string& color)  {
  if (color == color_) {
    crashed_ = false;
    car_tracker_->Reset();
  }
}


void Bot::TurboStarted(const std::string& color) {
}

void Bot::TurboEnded(const std::string& color) {
}

}  // namespace kamikaze
}  // namespace bots
