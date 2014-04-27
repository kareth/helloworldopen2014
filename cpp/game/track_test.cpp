#include <iostream>

#include "game/track.h"
#include "game/position.h"
#include "gtest/gtest.h"
#include "jsoncons/json.hpp"

using jsoncons::json;

namespace game {
namespace {

class TrackParseJsonTest : public testing::Test {
 protected:
  void SetUp() {
    json game_init_json = json::parse_file("data/gameInit.json");
    const auto& track_json = game_init_json["data"]["race"]["track"];

    track_.ParseFromJson(track_json);
  }
  Track track_;
};

TEST_F(TrackParseJsonTest, ParsesIdAndName) {
  EXPECT_EQ("keimola", track_.id());
  EXPECT_EQ("Keimola", track_.name());
}

TEST_F(TrackParseJsonTest, ParsesPieces) {
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

TEST_F(TrackParseJsonTest, ParsesLanes) {
  ASSERT_EQ(2, track_.lanes().size());

  EXPECT_EQ(-10, track_.lanes()[0].distance_from_center());
  EXPECT_EQ(0, track_.lanes()[0].index());

  EXPECT_EQ(10, track_.lanes()[1].distance_from_center());
  EXPECT_EQ(1, track_.lanes()[1].index());
}

class LaneLengthTest : public TrackParseJsonTest {
 protected:
  const double kEps = 1e-9;
};

TEST_F(LaneLengthTest, StraightLane) {
  Position position;
  EXPECT_NEAR(100, track_.LaneLength(position), kEps);
}

TEST_F(LaneLengthTest, StraightLaneOtherLane) {
  Position position;
  position.set_start_lane(1);
  position.set_end_lane(1);
  EXPECT_NEAR(100, track_.LaneLength(position), kEps);
}

TEST_F(LaneLengthTest, StraightLaneSwitch) {
  Position position;
  position.set_piece(3);
  position.set_start_lane(0);
  position.set_end_lane(1);
  EXPECT_NEAR(102.060274992934, track_.LaneLength(position), 0.1);
}

TEST_F(LaneLengthTest, TurnInnerLane) {
  Position position;
  position.set_piece(4);
  EXPECT_NEAR(86.393797973719316, track_.LaneLength(position), kEps);
}

TEST_F(LaneLengthTest, TurnOuterLane) {
  Position position;
  position.set_piece(4);
  position.set_start_lane(1);
  position.set_end_lane(1);
  EXPECT_NEAR(70.685834705770347, track_.LaneLength(position), kEps);
}

TEST_F(LaneLengthTest, TurnSwitchLane1) {
  Position position;
  position.set_piece(29);
  position.set_start_lane(1);
  position.set_end_lane(0);
  EXPECT_NEAR(81.028059516719, track_.LaneLength(position), 1);
}

// NOTE: There is difference when switching from 0 -> 1 and 1 -> 0.
TEST_F(LaneLengthTest, TurnSwitchLane2) {
  Position position;
  position.set_piece(29);
  position.set_start_lane(0);
  position.set_end_lane(1);
  EXPECT_NEAR(81.029484142008, track_.LaneLength(position), 1);
}

TEST_F(LaneLengthTest, TurnSwitchLane3) {
  Position position;
  position.set_piece(8);
  position.set_start_lane(0);
  position.set_end_lane(1);
  EXPECT_NEAR(81.053904159305, track_.LaneLength(position), 1);
}

TEST_F(LaneLengthTest, TurnSwitchLane4) {
  Position position;
  position.set_piece(8);
  position.set_start_lane(1);
  position.set_end_lane(0);
  EXPECT_NEAR(81.054652206375, track_.LaneLength(position), 1);
}

TEST_F(TrackParseJsonTest, RegularBetween) {
  Position position;
  position.set_piece(10);
  int from = 8;
  int to = 15;
  EXPECT_EQ(true, track_.IsBetween(position, from, to));
}

TEST_F(TrackParseJsonTest, AfterCyclicBetween) {
  Position position;
  position.set_piece(0);
  int from = 8;
  int to = 2;
  EXPECT_EQ(true, track_.IsBetween(position, from, to));
}

TEST_F(TrackParseJsonTest, BeforeCyclicBetween) {
  Position position;
  position.set_piece(9);
  int from = 7;
  int to = 1;
  EXPECT_EQ(true, track_.IsBetween(position, from, to));
}

}  // anonymous namespace
}  // namespace game
