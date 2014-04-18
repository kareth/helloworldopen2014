#include <iostream>

#include "game/car_tracker.h"
#include "gtest/gtest.h"

namespace game {

class VelocityModelTest : public testing::Test {
 protected:
  VelocityModel velocity_model_;
};

TEST_F(VelocityModelTest, FinlandTrack) {
  EXPECT_FALSE(velocity_model_.IsReady());

  // Ignore when standing
  velocity_model_.Record(0.0, 0, 0.5);
  velocity_model_.Record(0.0, 0, 0.5);
  velocity_model_.Record(0.0, 0, 0.5);
  velocity_model_.Record(0.0, 0, 0.5);

  EXPECT_FALSE(velocity_model_.IsReady());

  velocity_model_.Record(0.1, 0, 0.5);

  EXPECT_FALSE(velocity_model_.IsReady());

  velocity_model_.Record(0.198, 0.1, 0.5);

  EXPECT_TRUE(velocity_model_.IsReady());
  EXPECT_DOUBLE_EQ(0.1, velocity_model_.Predict(0.0, 0.5));
  EXPECT_DOUBLE_EQ(0.198, velocity_model_.Predict(0.1, 0.5));
  EXPECT_DOUBLE_EQ(0.29404, velocity_model_.Predict(0.198, 0.5));
}

}  // namespace game
