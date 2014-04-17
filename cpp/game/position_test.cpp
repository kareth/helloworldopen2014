#include <iostream>

#include "game/position.h"
#include "gtest/gtest.h"
#include "jsoncons/json.hpp"

using jsoncons::json;

namespace game {

class PositonParseJsonTest : public testing::Test {
 protected:
  void SetUp() {
    json game_init_json = json::parse_file("data/carPositions.json");
    const auto& position_json = game_init_json["data"][1];

    color_ = position_.ParseFromJson(position_json);
  }
  std::string color_;
  Position position_;
};

TEST_F(PositonParseJsonTest, ParsesColor) {
  EXPECT_EQ("blue", color_);
}

TEST_F(PositonParseJsonTest, ParsesPiecePosition) {
  EXPECT_EQ(0, position_.piece());
  EXPECT_NEAR(20.0, position_.piece_distance(), 1e-5);

  EXPECT_EQ(1, position_.start_lane());
  EXPECT_EQ(1, position_.end_lane());

  EXPECT_EQ(0, position_.lap());
}

TEST_F(PositonParseJsonTest, ParsesAngle) {
  EXPECT_NEAR(45.0, position_.angle(), 1e-5);
}

}  // game
