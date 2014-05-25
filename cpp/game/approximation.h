#ifndef CPP_GAME_APPROXIMATION_H_
#define CPP_GAME_APPROXIMATION_H_

#include <map>
#include <cstdio>
#include <iostream>
#include <algorithm>
#include <iterator>
#include <string>
#include <vector>

using std::min;
using std::swap;
using std::vector;

namespace game {

// Solves following equation:
// x[0] * a[0][0] + x[1] * a[0][1] = b[0]
// x[0] * a[1][0] + x[1] * a[1][1] = b[1]
// x[0] * a[2][0] + x[1] * a[2][1] = b[1]
//
// Note: input vectors will be destroyed.
//
// Returns the maximum accuracy error.
double Approximation(const vector<vector<double>>& a, const vector<double>& b, vector<double>& x);

}  // namespace game

#endif  // CPP_GAME_APPROXIMATION_H_
