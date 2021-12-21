#include <Rcpp.h>
using namespace Rcpp;

//' @title rbivariate_c 
//' @description Generate a bivariate sampler using Rcpp
//' @param N the number of samples
//' @param n the parameter
//' @param a the parameter
//' @param b the parameter
//' @return a random sample matrix of size N
//' @examples
//' \dontrun{
//' rbivariate_c(1000, 25, 1, 1)
//' }
//' @export
// [[Rcpp::export]]

NumericMatrix rbivariate_c(int N, int n, double a, double b) {
  NumericMatrix mat(N, 2);
  NumericVector v=(0, 0.5);         // initial value
  int burn = 1000;      // burn-in length
  mat(0,_) = v; 
  for(int i = 1; i < N; i++) {
    double X2 = mat(i-1,1);
    mat(i,0) = as<int>(rbinom(1, n, X2));
    int X1 = mat(i,0);
    mat(i,1) = as<double>(rbeta(1, X1 + a, n - X1 + b));
  }
  return(mat(Range(burn, N-1), _));
}
