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
  if (car_tracker_->positions().size() == 0)
    previous = position;
  else
    previous = car_tracker_->positions().back();

  car_tracker_->Record(position);

  if (crashed_) {
    return Command(0);
  }

  double throttle;
  if (position.lap() == 0) {
    throttle = 1;
  } else if (position.lap() == 1) {
    throttle = 0.6;
  } else {
    throttle = 0.8;
  }

  //throttle = Optimize(previous, position);

  //auto predicted = car_tracker_->Predict(position, previous, throttle, 0);

  car_tracker_->RecordThrottle(throttle);
  return Command(throttle);
}

double Bot::Optimize(const Position& previous, const Position& current) {
  int window_size = 70;
  vector<double> t(window_size, 0.9);
  vector<Position> positions {previous, current};

  for (int i = 0; i < window_size; i++) {
    Position predicted = car_tracker_->Predict(positions[i+1], positions[i], t[i], 0);
    positions.push_back(predicted);

    if (fabs(predicted.angle()) >= 50) {
      bool found = false;
      for (int j = i; j >= 0; j--) {
        if (t[j] > 1e-5) {
          t[j] = fmax(0, t[j]-0.1);
          i = j-1;
          positions.resize(i + 3);
          found = true;
          break;
        }
      }
      if (found) {
        continue;
      } else {
        std::cout << "\n\n\n\nFAIL to stop drifting";
        for (int j = 0; j < positions.size(); j++)
          std::cout << "(" << positions[j].piece_distance() << ", " << positions[j].angle() << ")  ";
        std::cout << "\n\n\n\n";
        return 0;
      }
    }

    // wyrownaj.
    /*
    int pos = i;
    double sum = 0;
    while (pos > 0 && t[pos] < 1e-5) pos--;
    while (pos > 0 && t[pos] > 1e-5) {sum += t[pos]; pos--; }
    if (t[pos] < 1e-5) pos++;
    else sum += t[pos];

    double ile = sum / double(i - pos + 1.0);

    for (int j = pos; j <= i; j++)
      t[j] = ile;
      */

  }

  double sum = 0;
  for(int i = 0; i < window_size; i++)
    sum += t[i];
  return sum/window_size;

  return t[0];
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
