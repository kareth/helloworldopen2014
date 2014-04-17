#include "game/track.h"
#include "gtest/gtest.h"
#include "jsoncons/json.hpp"

using jsoncons::json;

namespace game {

TEST(TrackTest, Foo) {
  json game_init_json = json::parse_file("data/gameInit.json");

  EXPECT_EQ(1, 1);
}

}  // game
