#ifndef CPP_GAME_SPAWN_MODEL_H_
#define CPP_GAME_SPAWN_MODEL_H_

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <algorithm>

#include "game/approximation.h"
#include "game/error_tracker.h"

namespace game {

// TODO check for +- 1 errors
class SpawnModel {
 public:
  SpawnModel();
  ~SpawnModel();

  void RecordCrash(int tick);
  void RecordSpawn(int tick);

  int duration() const { return duration_; }
  bool IsReady() const { return ready_; }

 private:
  int duration_ = 0;
  bool ready_ = false;
  int first_crash_tick_ = -1;
};

}  // namespace game

#endif  // CPP_GAME_SPAWN_MODEL_H_
