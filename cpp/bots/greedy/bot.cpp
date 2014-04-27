#include "bots/greedy/bot.h"
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
namespace greedy {

Bot::Bot() {
  srand(time(0));
}

game::Command Bot::GetMove(const map<string, Position>& positions, int game_tick)  {
  const Position& position = positions.at(color_);
  car_tracker_->Record(position);

  if (crashed_) {
    return Command(0);
  }

  const CarState& state = car_tracker_->current_state();
  double throttle = Optimize(state);

  car_tracker_->RecordCommand(Command(throttle));
  return Command(throttle);
}

double Bot::Optimize(const CarState& state) {
  int window_size = 120;
  vector<double> thr(window_size, 1);
  vector<CarState> states {state};

  for (int i = 0; i < window_size; i++) {
    CarState predicted = car_tracker_->Predict(states[i], Command(thr[i]));
    states.push_back(predicted);

    // TODO We should not hard code 60!
    if (!car_tracker_->crash_model().IsSafe(predicted.position().angle())) {
      bool found = false;
      // Look for something to slow down
      for (int j = i; j >= 0; j--) {
        states.pop_back();
        if (thr[j] > 1e-2) {
          thr[j] -= fmax(0, thr[j] - 0.005);
          i = j - 1;
          found = true;
          break;
        }
      }
      if (found) {
        continue;
      } else {
        std::cout << "FAILED to stop drifting" << std::endl;
        return 0;
      }
    }
  }
  return thr[0];
}

void Bot::JoinedGame() {
}

void Bot::YourCar(const string& color) {
  color_ = color;
}

void Bot::NewRace(const Race& race) {
  race_ = race;

  // We do not want to loose all models between qualification and race.
  // TODO We assume that the race is exactly the same as the one in car_tracker_.
  if (car_tracker_ == nullptr) {
    car_tracker_.reset(new CarTracker(&race_));
  }
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

void Bot::OnTurbo(const game::Turbo& turbo) {
}

}  // namespace greedy
}  // namespace bots
