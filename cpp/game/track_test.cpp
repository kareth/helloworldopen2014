#include <iostream>

#include "game/track.h"
#include "gtest/gtest.h"
#include "jsoncons/json.hpp"

using jsoncons::json;

namespace game {

class TrackTest : public testing::Test {
 protected:
  Track track_;
};

TEST_F(TrackTest, ParseFromJson) {
  json game_init_json = json::parse_file("data/gameInit.json");
  const auto& track_json = game_init_json["data"]["race"]["track"];

  track_.ParseFromJson(track_json);

  EXPECT_EQ("keimola", track_.id());
  EXPECT_EQ("Keimola", track_.name());

  ASSERT_EQ(123, track_.pieces().size());
}

}  // game
