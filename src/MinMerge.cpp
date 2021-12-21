#include <Rcpp.h>
using namespace Rcpp;

//' @title MinMerge
//' @description  Take the minimum of two vectors and merge them
//' @param x a vector
//' @param y a vector
//' @return a vector consisting of the minimum of the two vectors
//' @export
//[[Rcpp::export]]

NumericVector MinMerge(NumericVector x, NumericVector y) {
  int n = std::min(x.size(), y.size());
  NumericVector x1 = rep_len(x, n);
  NumericVector y1 = rep_len(y, n);
  NumericVector out(n);
  for (int i = 0; i < n; ++i) {
    out[i] = std::min(x1[i], y1[i]);
  }
  return out;
}
