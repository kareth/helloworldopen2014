#include <iostream>

#include "game/radius_model.h"
#include "gtest/gtest.h"

using jsoncons::json;

namespace game {

class RadiusModelTest : public testing::Test {
 protected:
  const double kEps = 1e-9;

  void SetUp() {
    json game_init_json = json::parse_file("data/gameInit.json");
    const auto& track_json = game_init_json["data"]["race"]["track"];

    track_.ParseFromJson(track_json);
    model_.reset(new RadiusModel(&track_));
  }

  Position position_;
  Track track_;
  std::unique_ptr<RadiusModel> model_;
};

TEST_F(RadiusModelTest, StraightLane) {
  EXPECT_DOUBLE_EQ(0.0, model_->Radius(position_));
}

TEST_F(RadiusModelTest, OuterLane) {
  position_.set_piece(4);
  EXPECT_DOUBLE_EQ(110.0, model_->Radius(position_));
}

TEST_F(RadiusModelTest, InnerLane) {
  position_.set_piece(4);
  position_.set_start_lane(1);
  position_.set_end_lane(1);
  EXPECT_DOUBLE_EQ(90.0, model_->Radius(position_));
}

TEST_F(RadiusModelTest, Switch) {
  vector<double> piece_distance{27.7685083163343000, 33.7680661019130000, 39.7676327317801000, 45.7672080290498000, 51.7667918203741000, 57.7663839358720000, 63.7659842090599000, 69.7655924767840000, 75.7652085791536000};
  vector<double> radius{107.4091129, 101.1829511, 95.36684499, 91.43855587, 88.60503624, 86.72509547, 86.27617138, 87.13189415, 89.0758595};

  position_.set_piece(29);
  position_.set_start_lane(1);
  position_.set_end_lane(0);

  // Learn only on every second piece
  for (int i = 0; i < piece_distance.size(); i += 2) {
    position_.set_piece_distance(piece_distance[i]);
    model_->Record(position_, radius[i]);
  }

  for (int i = 0; i < piece_distance.size(); ++i) {
    position_.set_piece_distance(piece_distance[i]);
    EXPECT_NEAR(radius[i], model_->Radius(position_) + 1, 1);

    // We should guarantee that estimated radius is smaller.
    EXPECT_GE(radius[i], model_->Radius(position_));
  }
}

}  // namespace game
