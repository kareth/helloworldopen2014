#include <iostream>

#include "game/car_tracker.h"
#include "game/race_tracker.h"
#include "gtest/gtest.h"

using jsoncons::json;

namespace game {

class IsSafeBehindTest : public ::testing::Test {
 protected:
  const double kEps = 1e-9;

  void SetUp() {
    json game_init_json = json::parse_file("data/gameInit.json");
    const auto& race_json = game_init_json["data"]["race"];

    race_.ParseFromJson(race_json);
    car_tracker_.reset(new CarTracker(&race_, PhysicsParams()));
    car_tracker_->mutable_crash_model()->force_angle(60.0);
    race_tracker_.reset(new RaceTracker(*car_tracker_, race_, "red"));
  }

  CarState Simulate(const Position& position,
                    double velocity,
                    int ticks) {
    CarState my_state(position);
    my_state.set_velocity(velocity);
    for (int i = 0; i < ticks; ++i) {
      my_state = car_tracker_->Predict(my_state, Command(0));
    }
    return my_state;
  }

  Race race_;
  std::unique_ptr<CarTracker> car_tracker_;
  std::unique_ptr<RaceTracker> race_tracker_;
};

}  // namespace game
