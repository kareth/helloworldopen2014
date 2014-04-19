#include <iostream>
#include <cmath>

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

TEST(GaussTest, Basic1) {
  vector<vector<double> > a{{0, 0.5}, {0.1, 0.5}};
  vector<double> b{0.1, -5};
  vector<double> x;
  GaussDouble(a, b, x);

  ASSERT_EQ(2, x.size());
  EXPECT_DOUBLE_EQ(-51, x[0]);
  EXPECT_DOUBLE_EQ(0.2, x[1]);
}

static double sin_degree(double x) {
  return sin(x / 180.0 * M_PI);
}

TEST(GaussTest, Oscilation) {
  vector<double> angle{28.7833, 28.2504, 27.5413, 26.6793, 25.6867, 24.5847, 23.3931, 22.1306, 20.8146, 19.461, 18.0847, 16.699, 15.3163, 13.9474, 12.602, 11.2888, 10.0152, 8.7876, 7.61134, 6.49087, 5.42971, 4.43054, 3.4953, 2.62518, 1.82074, 1.08195, 0.408251, -0.201395, -0.748441, -1.2347, -1.6623, -2.03364, -2.35132, -2.61812, -2.83698, -3.0109, -3.14296, -3.23628, -3.29397};

  // aa = sin(a) + va
  vector<vector<double>> a = {
    {sin_degree(angle[5]), angle[5], angle[4]},
    {sin_degree(angle[6]), angle[6], angle[5]},
    {sin_degree(angle[7]), angle[7], angle[6]}
  };
  vector<double> b{angle[6], angle[7], angle[8]};
  vector<double>x;

  GaussDouble(a, b, x);

  std::cout << "predicted,actual,error" << std::endl;
  for (int i = 6; i < angle.size() - 1; ++i) {
    double predicted = x[0] * sin_degree(angle[i-1]) + x[1] * angle[i-1] + x[2] * angle[i-2];
    std::cout << predicted << "," << angle[i] << "," << fabs(predicted - angle[i]) << std::endl;
  }

  std::cout << std::endl;
  std::cout << x[0] << "," << x[1] << "," << x[2] << "," << x[3] << std::endl;
}

}  // namespace game
