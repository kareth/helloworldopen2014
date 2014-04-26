#include <iostream>

#include "game/track.h"
#include "game/position.h"
#include "game/drift_model.h"
#include "gtest/gtest.h"
#include "jsoncons/json.hpp"

using jsoncons::json;

namespace game {
namespace {

class DriftModelTest : public testing::Test {
 protected:
  const double kEps = 1e-9;

  DriftModel model_;
};

TEST_F(DriftModelTest, EstimateRadius) {
  double angle = 1;
  double previous_angle = 0.5;
  double velocity = 6;
  double radius = 90;
  double direction = 1;
  double next_angle = model_.Predict(angle, previous_angle, velocity, radius, direction);

  EXPECT_NEAR(radius, model_.EstimateRadius(next_angle, angle, previous_angle, velocity, direction), kEps);
}

TEST_F(DriftModelTest, EstimateRadiusWithTooLowVelocity) {
  double angle = 1;
  double previous_angle = 0.5;
  double velocity = 1;
  double radius = 90;
  double direction = 1;
  double next_angle = model_.Predict(angle, previous_angle, velocity, radius, direction);

  EXPECT_NEAR(0, model_.EstimateRadius(next_angle, angle, previous_angle, velocity, direction), kEps);
}

}  // anonymous namespace
}  // namespace game
