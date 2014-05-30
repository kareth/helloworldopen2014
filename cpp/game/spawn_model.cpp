#include "game/spawn_model.h"

namespace game {

SpawnModel::SpawnModel() {
}

SpawnModel::~SpawnModel() {
  std::cout << "==== Spawn Model ====" << std::endl;
  std::cout << "time: " << duration_ << std::endl << std::endl;
  std::cout << std::endl;
}

void SpawnModel::RecordCrash(int tick) {
  if (first_crash_tick_ == -1)
    first_crash_tick_ = tick;
}

void SpawnModel::RecordSpawn(int tick) {
  if (!ready_) {
    ready_ = true;
    duration_ = tick - first_crash_tick_;
  }
}

}  // namespace game
