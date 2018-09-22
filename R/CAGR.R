#' # Compound Annual Growth Rate
#'
#' @param x xts object
#' @param m number to annualize (for monthly data, it will be 12, for dayli 256 ect.)
#'
#' @return Returns the Compound Annual Growth Rate
#' @export
#'
CAGR <- function(x, m){
  x <- na.omit(x)
  CAGR <- apply(x, 2, function(x, m) prod(1 + x)^(1 / (length(x) / m)) - 1, m = m)
  return(CAGR)
}
