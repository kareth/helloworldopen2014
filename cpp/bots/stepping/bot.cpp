#include "bots/stepping/bot.h"
#include "game/physics_params.h"
#include "utils/deadline.h"

DECLARE_int32(handicap);
DECLARE_bool(check_if_safe_ahead);
DECLARE_int32(answer_time); // in microseconds
DECLARE_bool(bump_with_turbo);
DECLARE_bool(defend_turbo_bump);
DECLARE_bool(write_switch_models);
DEFINE_bool(log_position, false, "");
DECLARE_bool(fastbanana_mode);

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
namespace stepping {

Bot::Bot() {
}

Bot::~Bot() {
  if (car_tracker_ != nullptr && FLAGS_write_switch_models) {
    car_tracker_->CreatePhysicsParams().Save();
  }
}

void Bot::LastTickApproved(const string& color, double time) {
  if (color == color_) {
    RecordCommand(last_command_);
    if (last_command_.ThrottleSet())
      last_throttle_ = last_command_.throttle();
  }
}

void Bot::LastTickIgnored(const string& color, double time) {
  if (color == color_) {
    if (!crashed_) {
      printf("Last tick missed! ;(((\n");
    }
    RecordCommand(game::Command(last_throttle_));
  }
}

void Bot::RecordCommand(const game::Command& command) {
  scheduler_->IssuedCommand(command);
  car_tracker_->RecordCommand(command);
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
    race_tracker_->ResurrectCars();
    race_tracker_->ResetBumpDetector();
  }

  scheduler_.reset(
      new schedulers::BulkScheduler(
        race_, *race_tracker_.get(), *car_tracker_.get()));
}

game::Command Bot::GetMove(const map<string, Position>& positions, int game_tick)  {
  game_tick_ = game_tick;
  const Position& position = positions.at(color_);

  double bump_velocity;
  bool bump = race_tracker_->HasSomeoneMaybeBumpedMe(positions, &bump_velocity);
  car_tracker_->Record(position, bump, bump_velocity);

  auto& state = car_tracker_->current_state();
  if (FLAGS_log_position) {
    std::cout << state.ShortDebugString() << std::endl;
  }

  if (game_tick < FLAGS_handicap) {
    return Command(0);
  }

  race_tracker_->Record(positions, game_tick);

  // TODO
  if (crashed_)
    return Command(0);

  if (FLAGS_fastbanana_mode) {
    if (race_.qualification_phase()) {  // Qual
      if (car_tracker_->IsReady()) {
        car_tracker_->mutable_crash_model()->force_angle(10.0);
      }
      // else normal throttle
    } else {  // Race
      if (game_tick < 3)
        return Command(0);
    }
  }

  Command command;
  SetStrategy(state);
  scheduler_->Schedule(state, game_tick, utils::Deadline(std::chrono::microseconds(FLAGS_answer_time)));
  command = scheduler_->command();

  last_command_ = command;
  return command;
}

void Bot::SetStrategy(const game::CarState& state) {
  // Race
  if (race_.laps() != -1) {
    scheduler_->set_strategy(Strategy::kOptimizeRace);
    return;
  // Qualification round, optimize every second lap
  } else {
    int lap = state.position().lap();

    if (lap % 2 == 1)
      scheduler_->set_strategy(Strategy::kOptimizeNextLap);
    else if (lap % 2 == 0 && lap != 0)
      scheduler_->set_strategy(Strategy::kOptimizeCurrentLap);
    else
      scheduler_->set_strategy(Strategy::kOptimizeRace);
  }
}

void Bot::ScheduleOvertakes() {
}

void Bot::OnTurbo(const game::Turbo& turbo) {
  race_tracker_->TurboForEveryone(turbo);

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
  crashed_ = false;
}

void Bot::CarFinishedLap(const string& color, const game::Result& result)  {
  race_tracker_->RecordLapTime(color, result.lap_time());

  printf("\x1B[35m*** %s Finished Lap %d in: %d millis ***\x1B[0m\n", color.c_str(), result.lap(), result.lap_time());
}

void Bot::CarFinishedRace(const string& color)  {
  race_tracker_->FinishedRace(color);
}

void Bot::GameEnd(/* results */)  {
}

void Bot::TournamentEnd() {
}

void Bot::CarCrashed(const string& color)  {
  race_tracker_->RecordCrash(color);
  car_tracker_->spawn_model().RecordCrash(game_tick_);

  if (color == color_) {
    auto& state = car_tracker_->current_state();
    auto next = car_tracker_->Predict(state, Command(car_tracker_->throttle()));
    printf("My crash! %lf %lf %lf\n", next.position().angle(), state.position().angle(), state.previous_angle());

    crashed_ = true;
    car_tracker_->RecordCarCrash();
  } else {
    printf("%s crashed!\n", color.c_str());
  }
}

void Bot::CarSpawned(const string& color)  {
  race_tracker_->CarSpawned(color);
  car_tracker_->spawn_model().RecordSpawn(game_tick_);
  if (color == color_) {
    crashed_ = false;
    car_tracker_->Reset();
  }
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
