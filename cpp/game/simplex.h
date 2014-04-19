#ifndef CPP_GAME_SIMPLEX_H_
#define CPP_GAME_SIMPLEX_H_

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

namespace Simplex {

// Maximizes following:
// A*x<=b, maksymalizujemy x*c (a[0][0]*x[0]+a[0][1]*x[1]+...<=b[0]), dla x>=0
// jak chcemy zeby xj moglo byc ujemne to podstawiamy zamiast xj dwie nowe
// zmiene (xj1-xj2), gdzie xj1,xj2>=0, funkcja zwraca najlepszy wektor, jesli
// rozwiazanie nie istnieje, lub jest nieograniczone, to zwracany jest pusty
// wektor
vector<double> simplex(vector<vector<double>> &A, vector<double> &b, vector<double> &c);

// Like gauss.
// x[0] * a[0][0] + x[1] * a[0][1] = b[0]
// x[0] * a[1][0] + x[1] * a[1][1] = b[1]
// x[0] * a[2][0] + x[1] * a[2][1] = b[1]
//
// Note: input vectors will be destroyed.
//
// a and b are input parameters, x is the output.
//
// Returns the error if couldn't find perfect solution.
double Optimize(vector<vector<double>>& a, vector<double>& b, vector<double>& x);

}  // namespace Simplex

#endif  // CPP_GAME_SIMPLEX_H_
