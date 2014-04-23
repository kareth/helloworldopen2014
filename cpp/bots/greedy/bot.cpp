#include "bots/greedy/bot.h"
#include <cstring>

using std::string;
using std::vector;
using std::map;

using game::Position;
using game::CarTracker;
using game::Race;
using game::Command;

namespace bots {
namespace greedy {

Bot::Bot() {
  srand(time(0));
}

game::Command Bot::GetMove(const map<string, Position>& positions, int game_tick)  {
  Position position = positions.at(color_);
  Position previous;

  if (car_tracker_->positions().size() == 0) {
    previous = position;
  } else {
    previous = car_tracker_->positions().back();
  }

  car_tracker_->Record(position);

  if (crashed_) {
    return Command(0);
  }

  double throttle = Optimize(previous, position);

  car_tracker_->RecordCommand(Command(throttle));
  return Command(throttle);
}

double Bot::Optimize(const Position& previous, const Position& current) {
  int window_size = 120;
  vector<double> thr(window_size, 1);
  vector<Position> positions {previous, current};

  for (int i = 0; i < window_size; i++) {
    Position predicted = car_tracker_->Predict(positions[i+1], positions[i], thr[i], 0);
    positions.push_back(predicted);

    if (fabs(predicted.angle()) >= 60) {
      bool found = false;
      // Look for something to slow down
      for (int j = i; j >= 0; j--) {
        positions.pop_back();
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
        std::cout << "FAILED to stop drifting";
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
  car_tracker_.reset(new CarTracker(&race_));
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
}

}  // namespace greedy
}  // namespace bots
