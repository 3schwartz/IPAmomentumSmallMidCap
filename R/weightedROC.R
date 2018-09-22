#' Weighted ROC
#'
#' @param x Object with returns
#' @param n Vector in which one choose up to three lookback periods
#' @param weights Vector in which one choose up to three weights
#'
#' @return Weighted ROC of returns
#' @export
#'
weightedROC <- function(x, n = c(1,3,6), weights = c(1/3, 1/3, 1/3)){
  roc1 <- TTR::ROC(x, n = n[1], type = "discrete")
  roc2 <- TTR::ROC(x, n = n[2], type = "discrete")
  roc3 <- TTR::ROC(x, n = n[3], type = "discrete")
  wave <- (roc1 * weights[1] + roc2 * weights[2] + roc3 * weights[3]) / sum(weights)
  return(wave)
}
