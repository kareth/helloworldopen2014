#ifndef CPP_UTILS_GAME_VISUALIZER_H_
#define CPP_UTILS_GAME_VISUALIZER_H_

#include <string>
#include <iostream>
#include <map>
#include "game/position.h"
#include "game/race.h"

#include "game/result.h"

namespace utils {

class GameVisualizer {
 public:
  GameVisualizer() : lap_times_() {}

  void Print() {
    if (print_) {
      if (visible_)
        CarriageReturn(positions_.size() * kPlayerLines);
      else
        visible_ = true;

      for (auto car : positions_)
        PrintCar(car.first, car.second);
    }
  }

  void Update(std::map<std::string, game::Position> positions) {
    positions_ = positions;
    Print();
  }

  void LapFinished(const game::Result& result) {
    if (lap_times_.find(result.color()) == lap_times_.end())
      lap_times_[result.color()] = vector<int>();

    lap_times_[result.color()].push_back(result.lap_time());
  }

  void CarCrashed(const std::string& color) {
    if (crashes_.find(color) == crashes_.end())
      crashes_[color] = vector<std::pair<int, int>>();

    crashes_[color].push_back(std::make_pair(positions_[color].lap(), positions_[color].piece()));
    Print();
  }

  void GameEnd() {
    lap_times_.clear();
    print_ = false;
    std::cout << std::endl;
  }

  void GameStart() { print_ = true; }

  void CarFinishedRace(const std::string& color) { Print(); }

  void set_race(const game::Race& race) {
    race_ = race;
    pieces_ = race_.track().pieces().size();
  }

 private:
  void PrintCar(const std::string& color, const game::Position& position) {
    std::cout << std::endl;
    PrintLapTimes(color);
    std::cout << std::endl;
    PrintTrackInfo(color, position);
    std::cout << std::endl;
  }

  void PrintTrackInfo(const std::string& color, const game::Position& position) {
    PrintPlayerName(color);
    std::cout << "|";

    for (int lap = 0; lap < position.lap(); lap++)
      PrintLap(color, lap, pieces_);

    PrintLap(color, position.lap(), position.piece() + 1);
  }

  void PrintLap(const std::string& color, int lap, int pieces) {
    for (int i = 0; i < pieces; i++) {
      if (IsCrash(color, lap, i))
        std::cout << "\x1B[31mX\x1B[0m";
      else
        std::cout << "_";
    }
    if (pieces == pieces_)
      std::cout << "|";
  }

  void PrintLapTimes(const std::string& color) {
    std::cout << std::setw(kHeaderLength) << " ";
    std::cout << "|";
    if (lap_times_.find(color) == lap_times_.end())
      return;

    for (int i = 0; i < lap_times_[color].size(); i++) {
      std::cout << std::right << std::setw(pieces_ / 2 + 3) <<
        std::setprecision(3) << double(lap_times_[color][i]) / 1000.0;
      std::cout << std::left << std::setw(pieces_ - pieces_ / 2 - 3) << "s" << "|";
      std::cout << std::defaultfloat;
    }
  }

  bool IsCrash(const std::string& color, int lap, int piece) {
    if (crashes_.find(color) == crashes_.end())
      return false;

    for (auto c : crashes_[color])
      if (c.first == lap && c.second == piece)
        return true;

    return false;
  }

  void CarriageReturn(int line_no) {
    while (--line_no)
      std::cout << "\x1b[A";
    std::cout << "\r";
    std::cout.flush();
  }

  void PrintPlayerName(const string& color) {
    std::map<std::string, std::string> colors {
      { "red", "\x1B[31m" },
      { "blue", "\x1B[34m" },
      { "green", "\x1B[32m" },
      { "normal", "\x1B[0m" }
    };

    for (auto c : race_.cars())
      if (c.color() == color) {
        std::cout << colors[color];
        std::string name = c.name();
        name[0] += 'A'-'a';

        int left = (kHeaderLength + name.size()) / 2;
        int right = kHeaderLength - left;

        std::cout << std::right << std::setw(left) << name;
        std::cout << std::left << std::setw(right) << " ";

        std::cout << colors["normal"];
      }
  }

  const int kPlayerLines = 4;
  const int kHeaderLength = 16;
  bool visible_ = false;
  bool print_ = false;

  game::Race race_;
  int pieces_;
  std::map<std::string, std::vector<int>> lap_times_;
  std::map<std::string, std::vector<std::pair<int, int>>> crashes_;
  std::map<std::string, game::Position> positions_;
};

}  // namespace utils

#endif  // CPP_UTILS_GAME_VISUALIZER_H_
