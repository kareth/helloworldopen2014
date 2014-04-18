#include <iostream>

#include "game/gauss.h"
#include "gtest/gtest.h"

using std::vector;

namespace game {

TEST(GaussTest, Basic) {
  vector<vector<double> > a{{0, 0.5}, {0.1, 0.5}};
  vector<double> b{0.1, 0.198};
  vector<double> x;
  GaussDouble(a, b, x);

  ASSERT_EQ(2, x.size());
  EXPECT_DOUBLE_EQ(0.98, x[0]);
  EXPECT_DOUBLE_EQ(0.2, x[1]);
}

}  // namespace game
