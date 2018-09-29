#include <Rcpp.h>
using namespace Rcpp;

//' @title weightsPropMatrix
//' @description
//' Returns weights given t-statistics and a confidence level
//' @name weightsPropMatrix
//' @param x A numeric matrix of t-statistics
//' @param cf double confidence level
//' @param k int lockback period
//'
//' @export

// [[Rcpp::export]]
NumericMatrix weightsPropMatrix(NumericMatrix X, double cf, int k) {
  int m = X.nrow(), n = X.ncol();

  NumericMatrix out(m, n);
  std::fill( out.begin(), out.end(), NumericVector::get_na() );

  for(int j = 0; j < n; j ++) {
    out(k-2,j) = 0;
    int km = (k-1);

    for(int i = km; i < m; i++) {
      if(X(i,j) > cf) {
        out(i,j) = 1;
      } else if(1 - X(i,j) > cf) {
        out(i,j) = 0;
      } else {
        out(i,j) = out(i-1,j);
      }
    }
  }
  return out;
}
