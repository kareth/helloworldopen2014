#include <iostream>
#include <cmath>

#include "game/simplex.h"
#include "gtest/gtest.h"

using std::vector;

TEST(SimplexTest, Basic) {
  vector<vector<double>> a {
    {-0.5, -1, 2},
    {1, 2, 0}};
  vector<double> b{-2, 100};
  vector<double> c{5, -1.5, 0.1};

  vector<double> res = Simplex::simplex(a, b, c);

  ASSERT_EQ(3, res.size());
  EXPECT_DOUBLE_EQ(100.0, res[0]);
  EXPECT_DOUBLE_EQ(0.0, res[1]);
  EXPECT_DOUBLE_EQ(24.0, res[2]);
}

TEST(SimplexTest, BasicOptimize) {
  vector<vector<double> > a{{0, 0.5}, {0.1, 0.5}};
  vector<double> b{0.1, 0.198};
  vector<double> x;

  Simplex::Optimize(a, b, x);

  ASSERT_EQ(2, x.size());
  EXPECT_DOUBLE_EQ(0.98, x[0]);
  EXPECT_DOUBLE_EQ(0.2, x[1]);
}

TEST(SimplexTest, BasicOptimizeWithNegative) {
  vector<vector<double> > a{{0, 0.5}, {0.1, 0.5}};
  vector<double> b{0.1, -5};
  vector<double> x;

  Simplex::Optimize(a, b, x);

  ASSERT_EQ(2, x.size());
  EXPECT_NEAR(-51, x[0], 1e-9);
  EXPECT_NEAR(0.2, x[1], 1e-9);
}

TEST(SimplexTest, OptimizeWithErrors) {
  vector<vector<double> > a{{0, 0.5}, {0.1, 0.5}, {0.3, 0.4}};
  vector<double> b{0.1, -5, 10};
  vector<double> x;

  Simplex::Optimize(a, b, x);

  ASSERT_EQ(2, x.size());
  EXPECT_NEAR(33.066666666666663, x[0], 1e-9);
  EXPECT_NEAR(0.2, x[1], 1e-9);
}

// .... + x - y
