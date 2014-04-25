#include "game/simplex.h"

#define REP(i, n) for (int i = 0; i < (n); ++i)
#define ALL(c) c.begin(),c.end()
#define SIZE(x) (int)x.size()

namespace Simplex{
/*jak chcemy zmienic typ, to zmieniamy typedef'a oraz wartosc EPS,
 na doublach tez powinno dzialac, ale troche szybciej i mniej dokladnie*/
#define EPS 1E-9
typedef double T;
typedef vector<T> VT;
typedef vector<int> VI;
vector<VT> A;
VT b,c,res;
VI kt,N;
int m;
inline void pivot(int k,int l,int e){
    int x=kt[l]; T p=A[l][e];
    REP(i,k) A[l][i]/=p; b[l]/=p; N[e]=0;
    REP(i,m) if (i!=l) b[i]-=A[i][e]*b[l],A[i][x]=A[i][e]*-A[l][x];
    REP(j,k) if (N[j]){
      c[j]-=c[e]*A[l][j];
      REP(i,m) if (i!=l) A[i][j]-=A[i][e]*A[l][j];
    }
    kt[l]=e; N[x]=1; c[x]=c[e]*-A[l][x];
}
/*k == #zmiennych bazowych + #zmiennych niebazowych*/
static VT doit(int k){
  VT res; T best;
  while (1){
    int e=-1,l=-1; REP(i,k) if (N[i] && c[i]>EPS) {e=i; break;}
    if (e==-1) break;
    REP(i,m) if (A[i][e]>EPS && (l==-1 || best>b[i]/A[i][e]))
      best=b[ l=i ]/A[i][e];
    if (l==-1) {
      // rozwiazanie nieograniczone,zwracam cokolwiek
      // std::cout << "Rozwiazanie nieograniczone" << std::endl;
      return VT();
    }
    pivot(k,l,e);
  }
  res.resize(k,0); REP(i,m) res[kt[i]]=b[i];
  return res;
}
/*A*x<=b, maksymalizujemy x*c (a[0][0]*x[0]+a[0][1]*x[1]+...<=b[0]), dla x>=0*/
/*jak chcemy zeby xj moglo byc ujemne to podstawiamy zamiast xj dwie nowe    */
/*zmiene (xj1-xj2), gdzie xj1,xj2>=0, funkcja zwraca najlepszy wektor, jesli */
/*rozwiazanie nie istnieje, lub jest nieograniczone, to zwracany jest pusty  */
/*wektor                                                                     */
VT simplex(vector<VT>& AA, VT& bb, VT& cc){
  int n=AA[0].size(),k;
  m=AA.size(); k=n+m+1; kt.resize(m); b=bb; c=cc; c.resize(n+m);
  A=AA; REP(i,m){ A[i].resize(k); A[i][n+i]=1; A[i][k-1]=-1; kt[i]=n+i;}
  N=VI(k,1); REP(i,m) N[kt[i]]=0;
  int pos=min_element(ALL(b))-b.begin();
  if (b[pos]<-EPS){ /*uwaga na epsilony*/
    c=VT(k,0); c[k-1]=-1; pivot(k,pos,k-1); res=doit(k);
    if (res[k-1]>EPS) /*brak rozwiazan*/ { return VT(); }
    REP(i,m) if (kt[i]==k-1)
        REP(j,k-1) if (N[j] && (A[i][j]<-EPS || EPS<A[i][j])){
          pivot(k,i,j); break;
        }
    c=cc; c.resize(k,0); REP(i,m) REP(j,k) if (N[j]) c[j]-=c[kt[i]]*A[i][j];
  }
  res=doit(k-1); if (!res.empty()) res.resize(n);
  return res;
}

double Optimize(const vector<vector<double>>& a, vector<double>& b, vector<double>& x) {
  vector<vector<double>> A;
  vector<double> B;

  // a[0].size() - number of variables required for result, *2 because (x0-x1)
  // a.size()    - number of variables used for errors, *2 because (s0 - s1)
  int num_variables = a[0].size() * 2 + a.size() * 2;

  // (x0 - x1) * a00 + (x2 - x3) * a01 + ... + s0 - s1 = b0
  // (x0 - x1) * a10 + (x2 - x3) * a01 + ... + s2 - s3 = b1

  for (int i = 0; i < a.size(); ++i) {
    // Ax <= b
    A.push_back(vector<double>(num_variables, 0));
    for (int j = 0; j < a[i].size(); ++j) {
      A.back()[j * 2] = a[i][j];
      A.back()[j * 2 + 1] = -a[i][j];
    }
    A.back()[a[0].size()*2 + i * 2] = 1;
    A.back()[a[0].size()*2 + i * 2 + 1] = -1;
    B.push_back(b[i]);

    // Now the same but with reverse sign.
    // Ax >= b (-Ax <= -b)
    A.push_back(A.back());
    for (int j = 0; j < A.back().size(); ++j) A.back()[j] = - A.back()[j];
    B.push_back(-b[i]);
  }

  vector<double> C(num_variables, 0);
  for (int i = a[0].size() * 2; i < num_variables; ++i) {
    C[i] = -1;
  }

  vector<double> res = simplex(A, B, C);

  // No solution :(
  if (res.size() == 0) {
    x = vector<double>(a[0].size(), 0);
    std::cout << "ERROR: simplex returned no solution ???" << std::endl;
    return -1;
  }

  // std::cout << "res: "; for (int i = 0; i < res.size(); ++i) { std::cout << res[i] << ", "; } std::cout << std::endl;
  // std::cout << "C: "; for (int i = 0; i < C.size(); ++i) { std::cout << C[i] << ", "; } std::cout << std::endl;

  double error = 0;
  for (int i = 0; i < res.size(); ++i) error += res[i] * C[i];

  for (int i = 0; i < a[0].size(); ++i) {
    x.push_back(res[i * 2] - res[i * 2 + 1]);
  }

  return error;
}


}  // namespace Simplex
