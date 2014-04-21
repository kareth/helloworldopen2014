#include "bots/kareth/bot.h"
#include <cstring>

using std::string;
using std::vector;
using std::map;

using game::Position;
using game::CarTracker;
using game::Race;
using game::Command;

namespace bots {
namespace kareth {

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

  //double throttle = 0.60 + double(rand() % 40)/99.0;

  double throttle = Optimize(previous, position);
  //double throttle = BinaryPossibilitiesOptimize(previous, position);
  if (game_tick < 10) throttle = 1;

  //throttle = 1;
  //if (car_tracker_->velocity() > 6)
  //  throttle = 0.6;


  //auto predicted = car_tracker_->Predict(position, previous, throttle, 0);

  car_tracker_->RecordThrottle(throttle);
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

double Bot::BinaryPossibilitiesOptimize(const Position& previous, const Position& current) {
  int groups = 17;
  int group_size = 1;

  int best = 0;
  double best_value = 0;
  double throttle;

  vector<Position> positions;

  for (int mask = 0; mask < (1 << groups); mask++) {
    positions.clear();
    positions.push_back(previous);
    positions.push_back(current);
    int fail = false;
    double distance = 0;

    for (int g = 0; g < groups; g++) {
      if (mask & (1 << g))
        throttle = 1;
      else
        throttle = 0;

      for (int t = 0; t < group_size; t++) {
        auto predicted = car_tracker_->Predict(positions.back(), positions[positions.size() - 2], throttle, 0);
        positions.push_back(predicted);
        distance += race_.track().Distance(positions.back(), positions[positions.size() - 2]);
        if (positions.back().angle() >= 55) {
          fail = true;
          break;
        }
      }

      if (fail == true)
        break;
    }

    if (fail == false) {
      if (distance > best_value) {
        best = mask;
        best_value = distance;
      }
    }
  }
  for (int i = 0; i < groups; i++)
    std::cout << ((best & (1 << i)) > 0) << " ";
  std::cout << "(" << current.piece() << ")" << std::endl;

  return (best & 1);
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

}  // namespace kareth
}  // namespace bots
