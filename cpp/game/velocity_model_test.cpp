#include <iostream>

#include "game/velocity_model.h"
#include "gtest/gtest.h"

namespace game {

class VelocityModelTest : public testing::Test {
 protected:
  VelocityModelTest() : velocity_model_(VelocityModelParams()) {}

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

TEST_F(VelocityModelTest, PredictThrottle) {
  velocity_model_.Record(0.1, 0, 0.5);
  velocity_model_.Record(0.198, 0.1, 0.5);

  ASSERT_TRUE(velocity_model_.IsReady());

  double v = 0.5;
  EXPECT_EQ(v, velocity_model_.Predict(v, velocity_model_.PredictThrottle(v)));
  v = 0.6;
  EXPECT_EQ(v, velocity_model_.Predict(v, velocity_model_.PredictThrottle(v)));
}

class BoundaryThrottleTest : public testing::Test {
 protected:
  BoundaryThrottleTest() : velocity_model_(VelocityModelParams()) {}

  double throttle_;
  VelocityModel velocity_model_;
};

TEST_F(BoundaryThrottleTest, GoingTooFast) {
  EXPECT_FALSE(velocity_model_.BoundaryThrottle(5, 1, &throttle_));
}

TEST_F(BoundaryThrottleTest, GoingTooSlow) {
  EXPECT_TRUE(velocity_model_.BoundaryThrottle(5, 20, &throttle_));
  EXPECT_NEAR(1.0, throttle_, 1e-9);
}

TEST_F(BoundaryThrottleTest, CustomThrottle) {
  EXPECT_TRUE(velocity_model_.BoundaryThrottle(5, velocity_model_.Predict(5, 0.5), &throttle_));
  EXPECT_NEAR(0.5, throttle_, 1e-9);
}

}  // namespace game
