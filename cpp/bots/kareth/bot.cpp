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

  double throttle = Optimize(previous, position);


  //auto predicted = car_tracker_->Predict(position, previous, throttle, 0);

  car_tracker_->RecordThrottle(throttle);
  return Command(throttle);
}

double Bot::Optimize(const Position& previous, const Position& current) {
  int window_size = 80;
  vector<double> thr(window_size, 1);
  vector<Position> positions {previous, current};
  //std::cout << "start" << std::endl;
  int crash = -1;

  for (int i = 0; i < window_size; i++) {
    //std::cout << "p" << i << " " << thr[i] << " "<< thr.size() << std::endl;
    while (positions.size() > i + 2)
      positions.pop_back();

    Position predicted = car_tracker_->Predict(positions[i+1], positions[i], thr[i], 0);
    positions.push_back(predicted);

    if (fabs(predicted.angle()) >= 58) {
      crash = i;
      bool found = false;
      for (int j = i; j >= 0; j--) {
        if (thr[j] > 1e-2) {
          //std::cout << "found! " << j << " " << thr[j] << std::endl;
          thr[j] -= 0.2;
          if (thr[j] < 0) thr[j] = 0;
          i = j - 1;
          found = true;
          break;
        }
      }
      if (found) {
        continue;
      } else {
        std::cout << "\n\n\n\nFAIL to stop drifting (" << positions.size() << " " << i <<")";
        for (int j = 0; j < positions.size(); j++)
          std::cout << "(" << positions[j].piece_distance() << ", " << positions[j].angle() << ")  ";
        std::cout << "\n\n\n\n";
        return 0;
      }
    }

    if (crash != -1 && i >= crash) crash = -1;

    // wyrownaj.

    if (crash == -1) {
      int pos = i;
      double sum = 0;
      while (pos > 0 && thr[pos] < 1e-5) pos--;
      while (pos > 0 && thr[pos] > 1e-5) {sum += thr[pos]; pos--; }
      if (thr[pos] < 1e-5) pos++;
      else sum += thr[pos];

      double ile = sum / double(i - pos + 1.0);

      for (int j = pos; j <= i; j++)
        thr[j] = ile;
    }

  }

  /*
  double sum = 0;
  for(int i = 0; i < window_size; i++)
    sum += thr[i];

  return sum/double(window_size);*/

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

}  // namespace kareth
}  // namespace bots
