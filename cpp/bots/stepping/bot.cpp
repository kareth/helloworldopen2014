#include "bots/stepping/bot.h"
#include <cstring>

using std::string;
using std::vector;
using std::map;

using game::Position;
using game::CarTracker;
using game::Race;
using game::Command;

namespace bots {
namespace stepping {

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
  if (game_tick < 10) throttle = 1;

  car_tracker_->RecordThrottle(throttle);
  return Command(throttle);
}

// Returns optimal throttlle:
double Bot::Optimize(const Position& previous, const Position& current) {
  // Length of time units in 0/1 search
  vector<int> groups
      { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 };
  //    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19  <-- counter

  // Optimal
  // { 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 } 7.43 keimola

  double distance;
  double best_distance = 0;
  double throttle = 0;
  int best_mask = 0;

  double l = 0, r = 1, m;

  // Find most optimal first tick

  while (r - l > 2e-1) {
    m = (l + r) / 2.0;

    Position next = car_tracker_->Predict(current, previous, m, 0);
    int mask = FindBestMask(current, next, groups, &distance);
    distance += race_.track().Distance(next, current);

    if (mask == -1) {
      r = m;
    } else {
      l = m;
      if (distance > best_distance) {
        best_distance = distance;
        throttle = m;
        best_mask = mask;
      }
    }
  }

  // Check fullspeed
  Position next = car_tracker_->Predict(current, previous, 1, 0);
  int mask = FindBestMask(current, next, groups, &distance);
  distance += race_.track().Distance(next, current);
  if (mask != -1 && distance > best_distance) {
    throttle = mask & 1;
    best_mask = mask;
  }

  // Check no-speed
  next = car_tracker_->Predict(current, previous, 0, 0);
  mask = FindBestMask(current, next, groups, &distance);
  distance += race_.track().Distance(next, current);
  if (mask != -1 && distance > best_distance) {
    throttle = mask & 1;
    best_mask = mask;
  }

  // Log predictions
  std::cout << std::setw(12) << throttle << " ";
  for (int i = 0; i < groups.size(); i++)
    std::cout << ((best_mask & (1 << i)) > 0) << " ";
  std::cout << "(" << current.piece() << ")" << std::endl;

  return throttle;
}

// Finds most optimal(by means of distance travelled) mask
// @returns mask or -1 if impossible
// @param distance total distance travelled
int Bot::FindBestMask(const Position& previous, const Position& current, const vector<int>& groups, double* distance) {
  int best_mask = -1;
  *distance = 0;

  for (int mask = 0; mask < (1 << (groups.size())); mask++) {
    double mask_distance;
    if (CheckMask(mask, previous, current, groups, &mask_distance) && mask_distance > *distance) {
      *distance = mask_distance;
      best_mask = mask;
    }
  }
  return best_mask;
}

// Checks whether given throttle setup wont crash
// @returns false if car crashes
// @param distance total distance travelled
bool Bot::CheckMask(int mask, const Position& previous, const Position& current, const vector<int>& groups, double* distance) {
  vector<Position> positions {previous, current};
  int now = 1;
  *distance = 0;

  for (int g = 0; g < groups.size(); g++) {
    for (int t = 0; t < groups[g]; t++) {
      positions[now ^ 1] = car_tracker_->Predict(positions[now], positions[now ^ 1], ((mask & (1 << g)) > 0), 0);
      now ^= 1;
      (*distance) += race_.track().Distance(positions[now], positions[now ^ 1]);
      if (fabs(positions[now].angle()) >= 60 - 1e-9)
        return false;
    }
  }
  return true;
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

}  // namespace stepping
}  // namespace bots
