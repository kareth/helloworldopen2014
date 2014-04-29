#include "bots/stepping/bot.h"

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
namespace stepping {

Bot::Bot() {
}

void Bot::NewRace(const Race& race) {
  race_ = race;
  // We do not want to loose all models between qualification and race.
  // TODO We assume that the race is exactly the same as the one in car_tracker_.
  // (kareth) Is resetting race enough? it only differs in laps/duration
  if (car_tracker_ == nullptr) {
    car_tracker_.reset(new CarTracker(&race_));
    race_tracker_.reset(new RaceTracker(*car_tracker_.get(), race_, color_));
    bump_tracker_.reset(new game::BumpTracker(*car_tracker_.get(), race_));
  } else {
    car_tracker_->set_race(&race_);
    race_tracker_->ResurrectCars();
  }

  scheduler_.reset(
      new schedulers::BulkScheduler(
        race_, *race_tracker_.get(), *car_tracker_.get(), FLAGS_answer_time));

  learning_scheduler_.reset(
      new schedulers::LearningScheduler(
        race_, *car_tracker_.get(), FLAGS_answer_time));
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

  Command command;
  if (car_tracker_->IsReady()) {
    SetStrategy(state);
    scheduler_->Schedule(state);
    command = scheduler_->command();

    if (FLAGS_defend_turbo_bump) {
      Command safe_command;
      if (!race_tracker_->IsSafeBehind(command, &safe_command)) {
        command = safe_command;

        Command safe_command;
        if (FLAGS_check_if_safe_ahead && !race_tracker_->IsSafeInFront(command, &safe_command)) {
          command = safe_command;
        }
      }
    }

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

void Bot::ScheduleOvertakes() {
}

void Bot::OnTurbo(const game::Turbo& turbo) {
  if (!crashed_) {
    car_tracker_->RecordTurboAvailable(turbo);
    printf("Turbo Available\n");
  }
  race_tracker_->TurboForEveryone(turbo);
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

  printf("\x1B[35m*** %s Finished Lap %d in: %d ticks ***\x1B[0m\n", color.c_str(), result.lap(), result.lap_time());
}

void Bot::CarFinishedRace(const string& color)  {
  race_tracker_->FinishedRace(color);
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
  race_tracker_->RecordCrash(color);
}

void Bot::CarSpawned(const string& color)  {
  if (color == color_) {
    crashed_ = false;
    car_tracker_->Reset();
  }
  race_tracker_->CarSpawned(color);
}

void Bot::CarDNF(const std::string& color) {
  race_tracker_->DNF(color);
}

void Bot::TurboStarted(const std::string& color) {
  race_tracker_->TurboStarted(color);
}

void Bot::TurboEnded(const std::string& color) {
}

}  // namespace stepping
}  // namespace bots
