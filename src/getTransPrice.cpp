#include <Rcpp.h>
using namespace Rcpp;

//' @title getTransPrice
//' @description
//' Returns trades
//' @name getTransPrice
//' @param X matrix with weights
//'
//' @export

// [[Rcpp::export]]
NumericMatrix getTransPrice(NumericMatrix X, NumericMatrix Y) {
  int m = X.nrow(), n = X.ncol();

  NumericMatrix out(m, n);
  std::fill( out.begin(), out.end(), NumericVector::get_na() );

  for(int j = 0; j < n; j ++) {
    for(int i = 0; i < m; i++) {
      out(i,j) = X(i,j) * Y(i,j);
    }
  }
  return out;
}
