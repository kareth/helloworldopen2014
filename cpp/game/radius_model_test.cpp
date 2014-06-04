#include <iostream>

#include "game/radius_model.h"
#include "gtest/gtest.h"
#include "gflags/gflags.h"

DECLARE_string(data_dir);

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

TEST_F(RadiusModelTest, ValidateRadiusEstimation) {
  FLAGS_data_dir = "../../data";
  SwitchRadiusParams data;
  data.Load();

  // Make sure we loaded some data.
  ASSERT_GT(data.model.size(), 100);

  std::map<std::tuple<double, double, double>, std::map<int, double> > d;

  for (const auto& it : data.model) {
    double start_radius = std::get<0>(it.first);
    double end_radius = std::get<1>(it.first);
    double angle = std::get<2>(it.first);
    int percent = std::get<3>(it.first);
    double radius = it.second;

    d[std::make_tuple(start_radius, end_radius, angle)][percent] = radius;
  }

  for (const auto& it : d) {
    double start_radius = std::get<0>(it.first);
    double end_radius = std::get<1>(it.first);
    double angle = std::get<2>(it.first);

    std::vector<double> radi = QuadraticBezierCurve::CreateForSwitch(
        start_radius, end_radius, angle).EstimateRadiuses();

    for (const auto& p : it.second) {
      EXPECT_NEAR(p.second, radi[p.first], 0.03);
    }
  }
}

TEST_F(RadiusModelTest, FirstWeEstimateThenUsePerfect) {
  FLAGS_data_dir = "../../data";
  SwitchRadiusParams data;
  data.Load();

  double length = 81.053904159305176336;
  // Create position for switch piece (210, 190)
  Position position;
  position.set_piece(8);
  position.set_start_lane(0);
  position.set_end_lane(1);

  EXPECT_NEAR(0.0, model_->Radius(position), 1e-10);

  for (int percent = 1; percent < 100; ++percent) {
    position.set_piece_distance(length * (percent + 0.5) / 100.0);

    double perfect_radius = data.model[std::make_tuple(210, 190, 22.5, percent)];

    EXPECT_NEAR(perfect_radius, model_->Radius(position) + SwitchRadiusModel::kSafetyMargin, 0.03);
    // Make sure we always return smaller radius that it actually is.
    EXPECT_GT(perfect_radius, model_->Radius(position));

    model_->Record(position, perfect_radius);

    EXPECT_NEAR(perfect_radius, model_->Radius(position), 1e-9);
  }
}

}  // namespace game
