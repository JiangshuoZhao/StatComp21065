#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix gibbsC(double a,double b,double n){
  int N = 10000;
  NumericMatrix X(N,2);
  X(0,0) = 0;
  X(0,1) = 0.5;
  double X1, X2;
  for(int i=1; i<N; i++){
    X2 = X(i-1,1);
    X(i,0) = rbinom(1,n,X2)[0];
    X1 = X(i,0);
    X(i,1) = rbeta(1,X1+a,n-X1+b)[0];
  }
  return X;
}
