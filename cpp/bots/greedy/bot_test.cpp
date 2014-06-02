#include "bots/greedy/bot.h"

#include <iostream>

#include "game/physics_params.h"
#include "game/car_tracker.h"
#include "game/command.h"
#include "game/race.h"
#include "game/position.h"
#include "game/position.h"
#include "gtest/gtest.h"

using jsoncons::json;
using game::Command;
using game::Race;
using game::CarTracker;
using game::Position;

namespace bots {
namespace greedy {

class BotTest : public testing::Test {
 protected:
  void SetUp() {
    json game_init_json = json::parse_file("../../game/data/gameInit.json");
    const auto& race_json = game_init_json["data"]["race"];

    race_.ParseFromJson(race_json);
    car_tracker_.reset(new CarTracker(&race_, game::PhysicsParams()));
  }

  Race race_;
  std::unique_ptr<CarTracker> car_tracker_;
};

TEST_F(BotTest, GreedyRun) {
  Bot bot;
  bot.NewRace(race_);
  bot.YourCar("red");
  bot.GetMove({{"red", Position()}}, 0);
}

}  // namespace greedy
}  // namespace bots
