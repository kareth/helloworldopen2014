#include <iostream>
#include <cmath>

#include "game/approximation.h"
#include "gtest/gtest.h"

using std::vector;

namespace game {

TEST(ApproximationTest, Basic) {
  vector<vector<double> > a{{0, 0.5}, {0.1, 0.5}};
  vector<double> b{0.1, 0.198};
  vector<double> x;
  Approximation(a, b, x);

  ASSERT_EQ(2, x.size());
  EXPECT_DOUBLE_EQ(0.98, x[0]);
  EXPECT_DOUBLE_EQ(0.2, x[1]);
}

TEST(ApproximationTest, Basic1) {
  vector<vector<double> > a{{0, 0.5}, {0.1, 0.5}};
  vector<double> b{0.1, -5};
  vector<double> x;
  Approximation(a, b, x);

  ASSERT_EQ(2, x.size());
  EXPECT_DOUBLE_EQ(-51, x[0]);
  EXPECT_DOUBLE_EQ(0.2, x[1]);
}


// Temporary tests used to find the switch model.

vector<double> model(double p) {
  double s = (p) / 100.0 * M_PI;
  // return {sin(s + M_PI/4.0), cos(s + M_PI/4.0), s * s, s, 1};
  return {s * s, s, 1};
  // return {sin(s), cos(s), 1};
  // return {exp(p/100), exp(-p/100), 1};
  // return {sinh(p / 100.0), 1};
}

double multiply(const vector<double>& a, const vector<double>& b) {
  double res = 0;
  for (int i = 0; i < a.size(); ++i) {
    res += a[i] * b[i];
  }

  return res;
}

TEST(ApproximationTest, Switch) {
  vector<double> percent{2 ,3 ,5 ,8 ,10 ,11 ,12 ,14 ,16 ,17 ,18 ,19 ,20 ,21 ,22 ,23 ,24 ,25 ,26 ,27 ,28 ,29 ,31 ,32 ,33 ,34 ,36 ,37 ,38 ,39 ,40 ,41 ,42 ,44 ,46 ,48 ,49 ,50 ,53 ,55 ,56 ,57 ,58 ,59 ,61 ,63 ,63 ,64 ,65 ,66 ,67 ,68 ,70 ,71 ,72 ,73 ,74 ,75 ,76 ,78 ,79 ,80 ,81 ,82 ,83 ,85 ,86 ,87 ,88 ,89 ,90 ,91 ,93 ,93 ,96 ,98 ,99};
  vector<double> radius{91.132424830579 ,90.673979800954 ,89.825326271739 ,88.723694711993 ,88.100152330066 ,87.822945327107 ,87.568684281696 ,87.128135817447 ,86.777969249032 ,86.637450015285 ,86.519377454142 ,86.424465598032 ,86.352088632174 ,86.302581895659 ,86.275878284786 ,86.272024734718 ,86.291020229034 ,86.332815784832 ,86.397476474581 ,86.484689300629 ,86.595029320648 ,86.727829274877 ,87.062491377790 ,87.263894563399 ,87.487516005339 ,87.733996979158 ,88.295797395716 ,88.611261065292 ,88.948743685726 ,89.309330325806 ,89.691835439004 ,90.097449303642 ,90.524899631360 ,91.447888816008 ,92.460824610447 ,93.561871756428 ,94.145604154269 ,94.750771969316 ,96.702946548696 ,98.114198193758 ,98.851744762041 ,99.613012100964 ,100.395517587090 ,101.199204666660 ,102.869857943758 ,104.630481663758 ,104.630481663759 ,105.542303021264 ,106.475089517330 ,107.428749487628 ,108.406561940652 ,109.401847437233 ,111.454417373568 ,112.515261689978 ,113.596753944419 ,114.698843603409 ,115.817526959576 ,116.960547145552 ,118.123972256998 ,120.511754694983 ,121.736001315736 ,122.984833539005 ,124.249367951339 ,125.533904290005 ,126.838381419582 ,129.506818605640 ,130.870649707872 ,132.259190586252 ,133.667458998552 ,135.095386201183 ,136.537544823482 ,137.999018861122 ,140.985212753493 ,140.985212753491 ,145.616737450779 ,148.799426700338 ,150.406623238444};

  ASSERT_EQ(percent.size(), radius.size());

  vector<vector<double> > a;
  vector<double> b = radius;
  vector<double> x;

  for (int i = 0; i < percent.size(); ++i) {
    a.push_back(model(percent[i]));
  }

  Approximation(a, b, x);

  double error = 0;
  double max_error = 0.0;
  for (int i = 0; i < percent.size(); ++i) {
    double e = fabs(b[i] - multiply(x, model(percent[i])));
    error += e;
    max_error = fmax(max_error, e);
  }
  error /= percent.size();


  std::cout << std::setprecision(20);
  std::cout << "accuracy = " << error << std::endl;
  std::cout << "max accuracy = " << max_error << std::endl;
  for (int i = 0; i < x.size(); i++)
    std::cout << "x" << i <<": " << x[i] << " ";
  std::cout << std::endl;
}

}  // namespace game
