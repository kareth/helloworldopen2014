#include <iostream>

#include "game/track.h"
#include "game/position.h"
#include "game/lane_length_model.h"
#include "game/physics_params.h"
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
    model_.reset(new LaneLengthModel(&track_, SwitchLengthParams()));
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

TEST_F(LaneLengthModelTest, SwitchOnTurn) {
  // Golden data
  vector<vector<double>> lengths = {
    {30,50,80,59.139983419378722829},
    {35,55,90,73.062075659429268626},
    {50,70,80,85.834968382374768225},
    {55,35,90,73.059669917775721615},
    {60,80,45,58.503066752223162439},
    {70,50,60,65.880506105711560849},
    {70,50,80,85.832707586219100904},
    {70,90,60,86.042955492246292692},
    {80,60,45,58.501693202337413879},
    {80,100,45,73.448262456320378533},
    {90,70,60,86.041132975104332559},
    {90,110,45,81.029484142008129766},
    {100,80,45,73.446849781627022935},
    {100,120,45,88.658173653634833045},
    {110,90,45,81.028059516718599298},
    {120,100,45,88.656740012435193421},
    {180,200,22.5,77.255183366417071511},
    {190,210,22.5,81.054652206374470325},
    {200,180,22.5,77.25443789318353538},
    {210,190,22.5,81.053904159305176336},
  };

  for (int i = 0; i < lengths.size(); ++i) {
    double length = model_->SwitchOnTurnLength(lengths[i][0], lengths[i][1], lengths[i][2]);
    EXPECT_NEAR(lengths[i][3], length, 1e-10);
  }
}

}  // anonymous namespace
}  // namespace game
