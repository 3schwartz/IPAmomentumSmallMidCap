#' Weighted Error adjusted momentum
#'
#' @param x Object with returns
#' @param n Vector in which one choose up to three lookback periods
#' @param weights Vector in which one choose up to three weights
#' @param period Periods at which one is rebalancing. Can be "days", "weeks", "months", "quarters" or "years"
#' @param SMAperiod Period in which one want to adjust prices. Hence this period is used when calculating forecast and thereby errors
#' @param CompareObj Object of which final object should be compare to. Ex. if period is monthly, then a frame with the wanted index should be provided
#'
#' @return Weighted Error adjusted momentum of returns
#' @export
#'
weightedErr <- function(x, n = c(1,3,6), weights = c(1/3, 1/3, 1/3),
                        period, SMAperiod, CompareObj){
  Err1 <- ErrMov(x, SMAperiod = SMAperiod, Lag = n[1], period = period, CompareObj = CompareObj)
  Err2 <- ErrMov(x, SMAperiod = SMAperiod, Lag = n[2], period = period, CompareObj = CompareObj)
  Err3 <- ErrMov(x, SMAperiod = SMAperiod, Lag = n[3], period = period, CompareObj = CompareObj)
  wave <- (Err1 * weights[1] + Err2 * weights[2] + Err3 * weights[3]) / sum(weights)
  return(wave)
}
