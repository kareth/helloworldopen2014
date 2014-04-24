#include <iostream>

#include "game/track.h"
#include "game/position.h"
#include "gtest/gtest.h"
#include "jsoncons/json.hpp"

using jsoncons::json;

namespace game {
namespace {

class LaneLengthModelTest : public testing::Test {
 protected:
  void SetUp() {
    json game_init_json = json::parse_file("data/gameInit.json");
    const auto& track_json = game_init_json["data"]["race"]["track"];

    track_.ParseFromJson(track_json);
    model_.reset(new LaneLengthModel(&track_));
  }

  Track track_;
  std::unique_ptr<LaneLengthModel> model_;
};
/*
TEST_F(LaneLengthModelTest, StraightLane) {
  Position position;
  EXPECT_NEAR(100, model_->Length(position), kEps);
}

TEST_F(LaneLengthModelTest, StraightLaneOtherLane) {
  Position position;
  position.set_start_lane(1);
  position.set_end_lane(1);
  EXPECT_NEAR(100, model_.Length(position), kEps);
}

TEST_F(LaneLengthModelTest, StraightLaneSwitch) {
  Position position;
  position.set_piece(3);
  position.set_start_lane(0);
  position.set_end_lane(1);
  EXPECT_NEAR(102.060274992934, track_.LaneLength(position), 0.1);
}

TEST_F(LaneLengthModelTest, TurnInnerLane) {
  Position position;
  position.set_piece(4);
  EXPECT_NEAR(86.393797973719316, track_.LaneLength(position), kEps);
}

TEST_F(LaneLengthModelTest, TurnOuterLane) {
  Position position;
  position.set_piece(4);
  position.set_start_lane(1);
  position.set_end_lane(1);
  EXPECT_NEAR(70.685834705770347, track_.LaneLength(position), kEps);
}

TEST_F(LaneLengthModelTest, TurnSwitchLane1) {
  Position position;
  position.set_piece(29);
  position.set_start_lane(1);
  position.set_end_lane(0);
  EXPECT_NEAR(81.028059516719, track_.LaneLength(position), 1);
}

// NOTE: There is difference when switching from 0 -> 1 and 1 -> 0.
TEST_F(LaneLengthModelTest, TurnSwitchLane2) {
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
}*/

}  // anonymous namespace
}  // namespace game
