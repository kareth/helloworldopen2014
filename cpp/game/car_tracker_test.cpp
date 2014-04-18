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

TEST(DriftTest, Basic) {
  // Data came from t=0.6 on Finland track
  vector<double> angle{0, 0, -0.20219, -0.585046, -1.12784, -1.81073, -2.61479, -3.52209, -4.51571, -5.57977, -6.69946, -7.86103, -9.05176, -10.26, -11.4752, -12.6876, -13.8887};
  vector<double> velocity{5.97162, 5.97162, 5.97274, 5.97329, 5.97382, 5.97434, 5.97486, 5.97536, 5.97585, 5.97634, 5.97681, 5.97727, 5.97773, 5.97773, 5.97861, 5.97904, 5.97946};

  DriftModel drift_model_;

  // Record 3 first values to train the model.
  for (int i = 0; i < 3; ++i) {
    EXPECT_FALSE(drift_model_.IsReady());
    drift_model_.Record(angle[i + 2], angle[i + 1], angle[i], velocity[i + 1]);
  }

  EXPECT_TRUE(drift_model_.IsReady());

  // Error should be less than 0.001
  for (int i = 5; i < angle.size(); ++i) {
    EXPECT_NEAR(angle[i], drift_model_.Predict(angle[i-1], angle[i-2], velocity[i-1]), 0.001);
  }
}

}  // namespace game
