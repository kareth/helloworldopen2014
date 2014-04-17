#include "bots/tomek/bot.h"

using game::Position;
using game::Command;
using game::Race;

namespace bots {
namespace tomek {

Bot::Bot() {
  stats_file_.open ("bin/stats.csv");
  stats_file_ << "piece_index,in_piece_distance,angle,velocity" << std::endl;
}

Bot::~Bot() {
  stats_file_.close();
}

game::Command Bot::GetMove(
    const std::map<std::string, Position>& positions)  {
  auto it = positions.find(color_);
  if (it == positions.end()) {
    std::cout << "Unknown position for color: " << color_ << std::endl;
    return Command(0);
  }
  auto& current_position = it->second;
  double velocity = 0;
  if (!positions_.empty()) {
    auto& last_position = positions_.back();
    if (last_position.piece() != current_position.piece()) {
      velocity = last_velocity_;
    } else {
      velocity = current_position.piece_distance() - last_position.piece_distance();
    }
  }
  last_velocity_ = velocity;

  stats_file_ << current_position.piece() << ","
              << current_position.piece_distance() << ","
              << current_position.angle() << ","
              << velocity << std::endl;

  positions_.push_back(current_position);
  return Command(0.6);
}

void Bot::JoinedGame() {
}

void Bot::YourCar(const std::string& color) {
  color_ = color;
}

void Bot::NewRace(const Race& race) {
  race_ = race;
}

void Bot::GameStarted() {
}

void Bot::CarFinishedLap(const std::string& color /* + results */)  {
}

void Bot::CarFinishedRace(const std::string& color)  {
}

void Bot::GameEnd(/* results */)  {
}

void Bot::TournamentEnd()  {
}

void Bot::CarCrashed(const std::string& color)  {
}

void Bot::CarSpawned(const std::string& color)  {
}

}  // namespace bots
}  // namespace tomek
