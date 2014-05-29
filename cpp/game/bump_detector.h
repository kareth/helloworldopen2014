#ifndef CPP_GAME_BUMP_DETECTOR_H_
#define CPP_GAME_BUMP_DETECTOR_H_

#include "game/car_tracker.h"
#include "game/enemy_tracker.h"
#include "game/race.h"

namespace game {

class BumpDetector {
 public:
  BumpDetector(CarTracker& car_tracker, const Race& race);

  void Record(const std::vector<EnemyTracker>& enemies, const std::map<std::string, Position>& positions);

  bool BumpOccured(const std::string& color, const std::string& color2);

 private:
  vector<std::pair<std::string, std::string>> bumps_;

  CarTracker& car_tracker_;
  const Race& race_;
};

}  // namespace game

#endif  // CPP_GAME_BUMP_DETECTOR_H_
