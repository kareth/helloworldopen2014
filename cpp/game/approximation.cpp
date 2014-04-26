#include <iostream>
#include <vector>
#include <cmath>
#include <algorithm>

using namespace std;
#define REP(i,n) for(int i=0;i<n;i++)
#define FOR(a,b,c) for(int a=b;a<=c;a++)

namespace game {

namespace {
typedef long double LD;
vector<double> InternalApproximation(int n, vector<vector<long double> >& tab){
  vector<double> res(n);
  REP(i,n-1){
    LD mx = tab[i][i];
    int mx_i = i;
    FOR(j, i + 1, n - 1) if (fabs(tab[j][i]) > fabs(mx))mx=tab[mx_i=j][i];
    if( mx_i != i )REP(j,n+1)swap(tab[i][j],tab[mx_i][j]);
    FOR(j,i+1,n-1){
      LD b = tab[j][i]/tab[i][i];
      FOR(c,i,n)tab[j][c]-=tab[i][c]*b;
    }
  }
  for(int i=n-1;i>0;i--){
    for(int r=i-1;r>=0;r--){
      LD b = tab[r][i]/tab[i][i];
      FOR(c,i,n)tab[r][c]-=tab[i][c]*b;
    }
  }
  REP(i,n)res[i]=tab[i][n]/tab[i][i];
  return res;
}
}  // anonymous namespace

void Approximation(const vector< vector<double> >& a, const vector<double>& b, vector<double>& x) {
  int k = a[0].size();
  int n = a.size();

  vector<vector<long double> > tab(k, vector<long double>(k + 1));
  REP(i,k)REP(j,k){
    tab[i][j]=0.0;
    REP(q,n)tab[i][j] += (long double) a[q][i] * a[q][j];
  }
  REP(i,k){
    tab[i][k]=0.0;
    REP(q,n)tab[i][k] += (long double) b[q] * a[q][i];
  }

  x = InternalApproximation(k, tab);
}

}  // namespace game
