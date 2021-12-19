#include <Rcpp.h>
using namespace Rcpp;

//' @title An Rcpp function for Exercise 9.8 (page 278, Statistical Computing with R).
//' @description An Rcpp function for Exercise 9.8 (page 278, Statistical Computing with R).
//' @param a A parameter the bivariate density.
//' @param b A parameter the bivariate density.
//' @param n A parameter the bivariate density.
//' @return A matrix with n*2.
//' @export
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
