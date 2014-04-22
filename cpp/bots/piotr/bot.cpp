#include "bots/piotr/bot.h"

using std::string;
using std::vector;
using std::map;

using game::Position;
using game::Command;
using game::Race;
using game::Piece;
using game::PieceType;

namespace bots {
namespace piotr {

Bot::Bot() {
}

void Bot::JoinedGame() {
  return;
}

void Bot::YourCar(const string& color) {
  return;
}

void Bot::NewRace(const Race& race) {
  race_ = race;
  speed_tracker_.reset(new physics::SpeedTracker(race));
  return;
}

void Bot::GameStarted() {
  return;
}

game::Command Bot::GetMove(const map<string, Position>& positions, int game_tick)  {
  speed_tracker_->Update(positions);

  const auto& my_pos = positions.begin()->second;

  double straight_speed = 1.0;
  double bent_speed = 0.65;
  double accel = 0.2;
  double top = 10.0;

  double angle;
  double dist = DistanceFromBent(my_pos, &angle);
  if (angle > 0) bent_speed += 0.066;

  double speed = speed_tracker_->SpeedFor(positions.begin()->first);
  double current_speed = speed;
  int can_brake = true;
  for (;;) {
    dist -= speed;
    speed = speed - accel * speed / top;
    if (dist < 0)
      can_brake = false;
    if (speed < bent_speed * top)
      break;
  }

  int straight = (race_.track().PieceFor(my_pos).type() == PieceType::kStraight);

  /*std::cout << "Straight? " << straight << "/" << angle <<
    "; Speed: " << current_speed << " can brake? " <<
    can_brake << std::endl;*/

  if (race_.track().PieceFor(my_pos).type() == PieceType::kStraight) {
    if (can_brake) {
      return Command(straight_speed);
    } else if (current_speed > bent_speed * top) {
      return Command(0.0);
    } else {
      return Command(1.0);
    }
  } else {
    if (current_speed > bent_speed * top)
      return Command(0);
    else
      return Command(1);
  }
}

double Bot::DistanceFromBent(const game::Position& position, double* angle) const {
  if (race_.track().PieceFor(position).type() == PieceType::kBent) {
    *angle = race_.track().PieceFor(position).angle();
    return 0;
  } else {
    double dist = race_.track().PieceFor(position).length() - position.piece_distance();
    for (int i = 1; ; i++) {
      if (race_.track().PieceFor(position, i).type() == PieceType::kBent) {
        *angle = race_.track().PieceFor(position, i).angle();
        break;
      }
      else
        dist += race_.track().PieceFor(position, i).length();
    }
    return dist;
  }
}

void Bot::CarFinishedLap(const string& color /* + results */)  {
  return;
}

void Bot::CarFinishedRace(const string& color)  {
  return;
}

void Bot::GameEnd(/* results */)  {
  return;
}

void Bot::TournamentEnd()  {
  return;
}

void Bot::CarCrashed(const string& color)  {
  return;
}

void Bot::CarSpawned(const string& color)  {
  return;
}

void Bot::OnTurbo(const game::Turbo& turbo) {
}

}  // namespace piotr
}  // namespace bots
