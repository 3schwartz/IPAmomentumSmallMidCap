#include <Rcpp.h>

using namespace Rcpp;
//' Simpel Moving Average with a Matrix as input
//'
//' @param X a matrix of which one want to calculate simpel moving average
//' @param period number one want to use when taking simpel moving average
//'
//' @export
// [[Rcpp::export]]

NumericMatrix SMAMatrixCpp(NumericMatrix X, int period) {
  int m = X.nrow(), n = X.ncol();

  NumericMatrix out(m, n);
  std::fill( out.begin(), out.end(), NumericVector::get_na() );

  for(int j = 0; j < n; j ++) {
    int km = (period - 1);
    for( int i = km; i < m; i++) {
      double val_total = 0;
      int im = i - km;
      for(int z = im; z <= i; z++) {
        val_total +=X(z,j);
      }
      out(i,j) = val_total / period;
    }
  }
  return out;
}
