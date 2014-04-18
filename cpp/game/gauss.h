#ifndef CPP_GAME_GAUSS_H_
#define CPP_GAME_GAUSS_H_

#include <map>
#include <cstdio>
#include <iostream>
#include <algorithm>
#include <iterator>
#include <string>
#include <vector>

#define FOR(v,p,k) for(int v=p;v<=k;++v)
#define FORD(v,p,k) for(int v=p;v>=k;--v)
#define REP(i,n) for(int i=0;i<(n);++i)

using std::min;
using std::swap;
using std::vector;

const double EPS = 1e-9;
inline bool IsZero(double x){ return x>=-EPS && x<=EPS; }


// Solves following equation:
// x[0] * a[0][0] + x[1] * a[0][1] = b[0]
// x[0] * a[1][0] + x[1] * a[1][1] = b[1]
// x[0] * a[2][0] + x[1] * a[2][1] = b[1]
//
// Note: input vectors will be destroyed.
static int GaussDouble(vector< vector<double> >& a, vector<double>& b, vector<double>& x){
  int m = a.size(), n = a[0].size(), k, r;
  vector<int> q;
  for (k = 0; k < min(m, n); k++) {
    int i, j;
    for (j = k; j < n; j++)
      for (i = k; i < m; i++)
        if (!IsZero(a[i][j])) goto found;
    break;

found:

    if (j != k) REP(t, m) swap(a[t][j], a[t][k]);
    q.push_back(j);
    if (i != k) {swap(a[i], a[k]); swap(b[i], b[k]);}
    FOR(j, k + 1, m - 1) if (!IsZero(a[j][k])){
      double l = (a[j][k] / a[k][k]);
      FOR(i, k + 1, n - 1) a[j][i] = a[j][i] - (l * a[k][i]);
      b[j] = b[j] - (l * b[k]);
    }
  }
  r = k; //r == rzad macierzy a
  x.clear(); x.resize(n, 0);
  FOR(k, r, m - 1) if (!IsZero(b[k])) return -1;
  FORD(k, r - 1, 0) {
    double s = b[k];
    FOR(j, k + 1, r - 1) s = s - (a[k][j] * x[j]);
    x[k] = s / a[k][k];
  }
  FORD(k, r - 1, 0) swap(x[k], x[q[k]]);
  return n - r;
}

#endif  // CPP_GAME_GAUSS_H_
