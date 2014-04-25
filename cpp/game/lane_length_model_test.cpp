#include <iostream>

#include "game/track.h"
#include "game/position.h"
#include "game/lane_length_model.h"
#include "gtest/gtest.h"
#include "jsoncons/json.hpp"

using jsoncons::json;

namespace game {
namespace {

class LaneLengthModelTest : public testing::Test {
 protected:
  const double kEps = 1e-9;

  void SetUp() {
    json game_init_json = json::parse_file("data/gameInit.json");
    const auto& track_json = game_init_json["data"]["race"]["track"];

    track_.ParseFromJson(track_json);
    model_.reset(new LaneLengthModel(&track_));
    perfect_ = false;
  }

  bool perfect_;
  Position position_;
  Track track_;
  std::unique_ptr<LaneLengthModel> model_;
};

TEST_F(LaneLengthModelTest, StraightLane) {
  EXPECT_NEAR(100, model_->Length(position_, &perfect_), kEps);
  EXPECT_TRUE(perfect_);
}

TEST_F(LaneLengthModelTest, StraightLaneOtherLane) {
  position_.set_start_lane(1);
  position_.set_end_lane(1);
  EXPECT_NEAR(100, model_->Length(position_, &perfect_), kEps);
  EXPECT_TRUE(perfect_);
}

TEST_F(LaneLengthModelTest, StraightLaneSwitch) {
  position_.set_piece(3);
  position_.set_start_lane(0);
  position_.set_end_lane(1);
  EXPECT_NEAR(102.060274992934, model_->Length(position_, &perfect_), 0.1);
  EXPECT_FALSE(perfect_);
}

TEST_F(LaneLengthModelTest, TurnInnerLane) {
  position_.set_piece(4);
  EXPECT_NEAR(86.393797973719316, model_->Length(position_, &perfect_), kEps);
  EXPECT_TRUE(perfect_);
}

TEST_F(LaneLengthModelTest, TurnOuterLane) {
  position_.set_piece(4);
  position_.set_start_lane(1);
  position_.set_end_lane(1);
  EXPECT_NEAR(70.685834705770347, model_->Length(position_, &perfect_), kEps);
  EXPECT_TRUE(perfect_);
}

TEST_F(LaneLengthModelTest, TurnSwitchLane1) {
  Position position;
  position.set_piece(29);
  position.set_start_lane(1);
  position.set_end_lane(0);
  EXPECT_NEAR(81.028059516719, model_->Length(position, &perfect_), 3);
  EXPECT_FALSE(perfect_);
}

// NOTE: There is difference when switching from 0 -> 1 and 1 -> 0.
TEST_F(LaneLengthModelTest, TurnSwitchLane2) {
  Position position;
  position.set_piece(29);
  position.set_start_lane(0);
  position.set_end_lane(1);
  EXPECT_NEAR(81.029484142008, model_->Length(position, &perfect_), 3);
  EXPECT_FALSE(perfect_);
}

TEST_F(LaneLengthModelTest, TurnSwitchLane3) {
  Position position;
  position.set_piece(8);
  position.set_start_lane(0);
  position.set_end_lane(1);
  EXPECT_NEAR(81.053904159305, model_->Length(position, &perfect_), 3);
  EXPECT_FALSE(perfect_);
}

TEST_F(LaneLengthModelTest, TurnSwitchLane4) {
  Position position;
  position.set_piece(8);
  position.set_start_lane(1);
  position.set_end_lane(0);
  EXPECT_NEAR(81.054652206375, model_->Length(position, &perfect_), 3);
  EXPECT_FALSE(perfect_);
}

TEST_F(LaneLengthModelTest, LearnTurnSwitchLength) {
  const double piece_length = 81.028059516719;

  Position previous;
  previous.set_piece(29);
  previous.set_start_lane(1);
  previous.set_end_lane(0);
  previous.set_piece_distance(80);

  Position current;
  current.set_piece(30);
  current.set_start_lane(0);
  current.set_end_lane(0);
  current.set_piece_distance(4 - 1.028059516719);

  model_->Record(previous, current, 4);

  Position position;
  position.set_piece(29);
  position.set_start_lane(1);
  position.set_end_lane(0);
  EXPECT_NEAR(piece_length, model_->Length(position, &perfect_), kEps);
  EXPECT_TRUE(perfect_);

  position.set_start_lane(0);
  position.set_end_lane(1);
  EXPECT_NEAR(piece_length, model_->Length(position, &perfect_), kEps);
  EXPECT_FALSE(perfect_);
}

TEST_F(LaneLengthModelTest, LearnStraightSwitchLength) {
  const double piece_length = 102.060274992934;

  Position previous;
  previous.set_piece(3);
  previous.set_start_lane(0);
  previous.set_end_lane(1);
  previous.set_piece_distance(100);

  Position current;
  current.set_piece(4);
  current.set_start_lane(1);
  current.set_end_lane(1);
  current.set_piece_distance(5 - 2.060274992934);

  model_->Record(previous, current, 5);

  Position position;
  position.set_piece(3);
  position.set_start_lane(0);
  position.set_end_lane(1);
  EXPECT_NEAR(piece_length, model_->Length(position, &perfect_), kEps);
  EXPECT_TRUE(perfect_);

  // On straight line both switches are the same.
  position.set_start_lane(1);
  position.set_end_lane(0);
  EXPECT_NEAR(piece_length, model_->Length(position, &perfect_), kEps);
  EXPECT_TRUE(perfect_);
}

}  // anonymous namespace
}  // namespace game
