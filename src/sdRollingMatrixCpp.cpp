#include <Rcpp.h>
#include "SMAMatrixCpp.h"

using namespace Rcpp;

//' @title sdRollingMatrixCpp
//' @description
//' SD rolling over specified period with matrix as output
//' @name sdRollingMatrixCpp
//' @param X a matrix of which one want to calculate SD rolling
//' @param period number one want to use when taking SD rolling
//'
//' @export
// [[Rcpp::export]]

NumericMatrix sdRollingMatrixCpp(NumericMatrix X, int period) {
  int m = X.nrow(), n = X.ncol();

  NumericMatrix mean = SMAMatrixCpp(X, period);

  NumericMatrix out(m, n);
  std::fill( out.begin(), out.end(), NumericVector::get_na() );

  for(int j = 0; j < n; j ++) {

    int km = (period - 1);

    for( int i = km; i < m; i++) {

      double val_total = 0;
      int im = i - km;

      for(int z = im; z <= i; z++) {
        val_total += pow(X(z,j) - mean(i,j), 2);
      }
      out(i,j) = sqrt(val_total / (period - 1));
    }
  }
  return out;
}
