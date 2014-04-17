#include <iostream>

#include "game/track.h"
#include "gtest/gtest.h"
#include "jsoncons/json.hpp"

using jsoncons::json;

namespace game {

class ParseJsonTest : public testing::Test {
 protected:
  void SetUp() {
    json game_init_json = json::parse_file("data/gameInit.json");
    const auto& track_json = game_init_json["data"]["race"]["track"];

    track_.ParseFromJson(track_json);
  }
  Track track_;
};

TEST_F(ParseJsonTest, ParsesIdAndName) {
  EXPECT_EQ("keimola", track_.id());
  EXPECT_EQ("Keimola", track_.name());
}

TEST_F(ParseJsonTest, ParsesPieces) {
  ASSERT_EQ(40, track_.pieces().size());

  EXPECT_EQ(PieceType::kStraight, track_.pieces()[0].type());
  EXPECT_EQ(100, track_.pieces()[0].length());
  EXPECT_EQ(false, track_.pieces()[0].has_switch());

  EXPECT_EQ(PieceType::kStraight, track_.pieces()[3].type());
  EXPECT_EQ(100, track_.pieces()[3].length());
  EXPECT_EQ(true, track_.pieces()[3].has_switch());

  EXPECT_EQ(PieceType::kBent, track_.pieces()[8].type());
  EXPECT_EQ(22.5, track_.pieces()[8].angle());
  EXPECT_EQ(200, track_.pieces()[8].radius());
  EXPECT_EQ(true, track_.pieces()[8].has_switch());
}

TEST_F(ParseJsonTest, ParsesLanes) {
  ASSERT_EQ(2, track_.lanes().size());

  EXPECT_EQ(-10, track_.lanes()[0].distance_from_center());
  EXPECT_EQ(0, track_.lanes()[0].index());

  EXPECT_EQ(10, track_.lanes()[1].distance_from_center());
  EXPECT_EQ(1, track_.lanes()[1].index());
}

}  // game
