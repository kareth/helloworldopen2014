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
  for (int i = 0; i < 5; ++i) {
    EXPECT_FALSE(drift_model_.IsReady());
    drift_model_.Record(angle[i + 2], angle[i + 1], angle[i], velocity[i + 1]);
  }

  EXPECT_TRUE(drift_model_.IsReady());

  // Error should be less than 0.001
  for (int i = 5; i < angle.size(); ++i) {
    EXPECT_NEAR(angle[i], drift_model_.Predict(angle[i-1], angle[i-2], velocity[i-1]), 1);
  }
}

TEST(DriftTest, Oscilation) {
  vector<double> angle{28.7833, 28.2504, 27.5413, 26.6793, 25.6867, 24.5847, 23.3931, 22.1306, 20.8146, 19.461, 18.0847, 16.699, 15.3163, 13.9474, 12.602, 11.2888, 10.0152, 8.7876, 7.61134, 6.49087, 5.42971, 4.43054, 3.4953, 2.62518, 1.82074, 1.08195, 0.408251, -0.201395, -0.748441, -1.2347, -1.6623, -2.03364, -2.35132, -2.61812, -2.83698, -3.0109, -3.14296, -3.23628, -3.29397, -3.31913, -3.31481, -3.28398, -3.22956, -3.15434, -3.06101, -2.95214, -2.83018, -2.69741, -2.55601, -2.40797, -2.25518, -2.09934, -1.94203, -1.78467, -1.62855, -1.47481, -1.32446, -1.17838, -1.03734, -0.901967, -0.772807, -0.650284, -0.53473, -0.426387, -0.325414, -0.231893, -0.145841, -0.0672093, 0.00410552, 0.0682555, 0.125436, 0.175879, 0.219849, 0.257636, 0.28955};

  DriftModel drift_model_;

  // Record 5 first values to train the model.
  for (int i = 0; i < 5; ++i) {
    EXPECT_FALSE(drift_model_.IsReady());
    drift_model_.Record(angle[i + 2], angle[i + 1], angle[i], 6.5 + i);
  }

  EXPECT_TRUE(drift_model_.IsReady());

  // Error should be less than 0.001
  for (int i = 7; i < angle.size(); ++i) {
    EXPECT_NEAR(angle[i], drift_model_.Predict(angle[i-1], angle[i-2], 6.5), 1) << i;
  }
}

}  // namespace game
