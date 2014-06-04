#ifndef CPP_GAME_PATH_OPTIMIZER_INTERFACE_H_
#define CPP_GAME_PATH_OPTIMIZER_INTERFACE_H_

#include "game/car_tracker.h"
#include "game/race.h"
#include <map>

namespace game {

class PathOptimizerInterface {
 public:
   // Returns all possible decisions with associated score
   // score means the loss of time (lap-wise) compared to optimal choice
   // so atleast one lane will always have value of 0
   virtual std::map<Switch, int> Score(const Position& position) = 0;

 private:
};

}  // namespace game

#endif  // CPP_GAME_PATH_OPTIMIZER_INTERFACE_H_
