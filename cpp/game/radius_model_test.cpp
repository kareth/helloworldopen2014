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

    lane_length_model_.reset(new LaneLengthModel(&track_, SwitchLengthParams()));
    track_.ParseFromJson(track_json);
    model_.reset(new RadiusModel(&track_, lane_length_model_.get(), SwitchRadiusParams()));
  }

  Position position_;
  Track track_;
  std::unique_ptr<RadiusModel> model_;
  std::unique_ptr<LaneLengthModel> lane_length_model_;
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

/*
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
*/
/*
class SwitchRadiusModelTest : public testing::Test {
 protected:
  SwitchRadiusModelTest() : model_(1, 2) {}

  SwitchRadiusModel model_;
};

TEST_F(SwitchRadiusModelTest, Switch2) {
  vector<double> piece_distance{5.455820072, 5.639723642, 11.78062607, 12.54728078, 18.17893595, 19.51668677, 24.64927963, 26.54670464, 31.19021644, 33.63612216, 37.80033451, 40.78375132, 44.47825022, 47.9884279, 51.22260761, 55.24901095, 58.03207786, 62.56438234, 64.9053587, 69.9334463, 71.84117393, 77.35512898, 78.83827285};
  vector<double> radius{89.43628325, 89.43628325, 87.12813582, 86.9418616, 86.27587828, 86.29102023, 86.88396473, 87.26389456, 88.94874369, 90.0974493, 92.46082461, 94.75077197, 97.39791135, 101.1992047, 104.6304817, 109.4018474, 112.5152617, 119.3077153, 122.9848335, 130.8706497, 133.667459, 144.0537967, 147.1986429};

  for (int i = 0; !model_.IsReady() && i < piece_distance.size(); ++i) {
    model_.Record(piece_distance[i], radius[i]);
  }

  ASSERT_TRUE(model_.IsReady());

  for (int i = 0; i < piece_distance.size(); ++i) {
    EXPECT_GE(radius[i], model_.Radius(piece_distance[i]));
  }
}
*/

}  // namespace game
