#include <iostream>

#include "game/velocity_predictor.h"
#include "gtest/gtest.h"

using jsoncons::json;

namespace game {

class VelocityPredictorTest : public testing::Test {
 protected:
  const double kEps = 1e-9;

  void SetUp() {
    json game_init_json = json::parse_file("data/gameInit.json");
    const auto& race_json = game_init_json["data"]["race"];

    race_.ParseFromJson(race_json);
    car_tracker_.reset(new CarTracker(&race_, PhysicsParams()));

    velocity_predictor_.reset(new VelocityPredictor(*car_tracker_, race_));
  }

  Position BuildPosition(int piece, double distance, int lane = 0) {
    Position position;
    position.set_piece(piece);
    position.set_piece_distance(distance);
    position.set_start_lane(lane);
    position.set_end_lane(lane);
    return position;
  }

  CarState BuildState(int piece, double distance, double velocity, int lane = 0) {
    CarState state(BuildPosition(piece, distance, lane));
    state.set_velocity(velocity);
    return state;
  }

  Race race_;
  std::unique_ptr<CarTracker> car_tracker_;
  std::unique_ptr<VelocityPredictor> velocity_predictor_;
};

TEST_F(VelocityPredictorTest, ShouldAddNewPoints) {
  velocity_predictor_->Record(BuildState(0, 0, 5));
  velocity_predictor_->Record(BuildState(0, 10, 10));

  EXPECT_NEAR(7.5, velocity_predictor_->Velocity(BuildPosition(0, 5)), kEps);
}

TEST_F(VelocityPredictorTest, ShouldInterpolateCorrectly) {
  velocity_predictor_->Record(BuildState(0, 0, 5));
  velocity_predictor_->Record(BuildState(0, 7.5, 7.5));
  velocity_predictor_->Record(BuildState(0, 20, 12.5));

  EXPECT_NEAR(6, velocity_predictor_->Velocity(BuildPosition(0, 3)), kEps);
  EXPECT_NEAR(10.5, velocity_predictor_->Velocity(BuildPosition(0, 15)), kEps);
}


TEST_F(VelocityPredictorTest, CurrentSpeedForUnknown) {
  velocity_predictor_->Record(BuildState(0, 0, 5));
  velocity_predictor_->Record(BuildState(0, 10, 10));

  EXPECT_NEAR(10, velocity_predictor_->Velocity(BuildPosition(9, 7)), kEps);
}

TEST_F(VelocityPredictorTest, ShouldNotOverwriteHigherSpeed) {
  velocity_predictor_->Record(BuildState(1, 5, 5));
  velocity_predictor_->Record(BuildState(1, 15, 10));
  velocity_predictor_->Record(BuildState(1, 30, 15));
  velocity_predictor_->Record(BuildState(1, 50, 20));

  EXPECT_NEAR(7.5, velocity_predictor_->Velocity(BuildPosition(1, 10)), kEps);
  EXPECT_NEAR(17.5, velocity_predictor_->Velocity(BuildPosition(1, 40)), kEps);

  velocity_predictor_->Record(BuildState(1, 10, 5));
  velocity_predictor_->Record(BuildState(1, 20, 10));
  velocity_predictor_->Record(BuildState(1, 35, 15));
  velocity_predictor_->Record(BuildState(1, 45, 20));

  EXPECT_NEAR(7.5, velocity_predictor_->Velocity(BuildPosition(1, 10)), kEps);
  EXPECT_NEAR(17.5, velocity_predictor_->Velocity(BuildPosition(1, 40)), kEps);
}


TEST_F(VelocityPredictorTest, ShouldSaveFasterPoints) {
  /*velocity_predictor_->Record(BuildState(2, 5, 5));
  velocity_predictor_->Record(BuildState(2, 15, 10));
  velocity_predictor_->Record(BuildState(2, 30, 15));
  velocity_predictor_->Record(BuildState(2, 50, 20));

  EXPECT_NEAR(7.5, velocity_predictor_->Velocity(BuildPosition(2, 10)), kEps);
  EXPECT_NEAR(11, velocity_predictor_->Velocity(BuildPosition(2, 18)), kEps);
  EXPECT_NEAR(17.5, velocity_predictor_->Velocity(BuildPosition(2, 40)), kEps);

  velocity_predictor_->Record(BuildState(2, 2, 0));
  velocity_predictor_->Record(BuildState(2, 12, 10));
  velocity_predictor_->Record(BuildState(2, 28, 16));
  velocity_predictor_->Record(BuildState(2, 30, 2));

  //Now points should be (2,0) (5,5) (12,10) (28,16) (30,15) (50,20)

  EXPECT_NEAR(, velocity_predictor_->Velocity(BuildPosition(2, 3)), kEps);
  EXPECT_NEAR(, velocity_predictor_->Velocity(BuildPosition(2, 10)), kEps);
  EXPECT_NEAR(, velocity_predictor_->Velocity(BuildPosition(2, 10)), kEps);
  EXPECT_NEAR(, velocity_predictor_->Velocity(BuildPosition(2, 10)), kEps);
  EXPECT_NEAR(, velocity_predictor_->Velocity(BuildPosition(2, 10)), kEps);
  EXPECT_NEAR(, velocity_predictor_->Velocity(BuildPosition(2, 10)), kEps);*/
}

}  // namespace game
