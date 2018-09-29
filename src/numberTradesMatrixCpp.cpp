#include <Rcpp.h>
using namespace Rcpp;

//' @title numberTradesMatrixCpp
//' @description
//' Returns trades
//' @name numberTradesMatrixCpp
//' @param X matrix with weights
//'
//' @export

// [[Rcpp::export]]
NumericMatrix numberTradesMatrixCpp(NumericMatrix X) {
  int m = X.nrow(), n = X.ncol();

  NumericMatrix out(m, n);
  std::fill( out.begin(), out.end(), NumericVector::get_na() );

  for(int j = 0; j < n; j ++) {
    out(0,j) = 0;
    for(int i = 1; i < m; i++) {
      if(((X(i,j) == 0) & (X(i-1,j) ==1)) | ((X(i-1,j) == 0) & (X(i,j) ==1))) {
        out(i,j) = 1;
      } else {
        out(i,j) = 0;
      }
    }
  }
  return out;
}
