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

  //double throttle = Optimize(previous, position);
  double throttle = BinaryPossibilitiesOptimize(previous, position);
  if (game_tick < 10) throttle = 1;

  //throttle = 1;
  //if (car_tracker_->velocity() > 6)
  //  throttle = 0.6;


  //auto predicted = car_tracker_->Predict(position, previous, throttle, 0);

  car_tracker_->RecordThrottle(throttle);
  return Command(throttle);
}

double Bot::Optimize(const Position& previous, const Position& current) {
  int window_size = 45;
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

    if (fabs(predicted.angle()) >= 57) {
      //std::cout << "crash" << std::endl;
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
        thr[j] = (ile + 4.0 * thr[j]) / 5.0;
    }
  }

  /*
  double sum = 0;
  for(int i = 0; i < window_size; i++)
    sum += thr[i];

  return sum/double(window_size);*/
  //std::cout << thr[0] << std::endl;

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
        if (positions.back().angle() >= 60) {
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
