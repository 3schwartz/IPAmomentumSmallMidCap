#' Relative Momentum Strategy
#'
#' @param xts.ret Object of returns
#' @param xts.rank Object of ranked returns
#' @param n Number of assets in portfolio. Has to be less than number of assets
#' @param ret.fill.na Number of NA'sto put in the beginning
#' @param TC Tranding costs. If null, then vector without trading cost will be returned
#'
#' @return  list
#'  \item{name mat}{Matrix of returns for top ranked assets}
#'  \item{name ret}{Vector of returns for tp ranked assets. If TC is not null, tranding cost is in the calculation}
#'  \item{name rank}{Matrix of ranks for the top}
#'  \item{name NumberTrades}{Scalar number of trades}
#' @export
#'
RelativeMomentum <- function(xts.ret, xts.rank, n = 1, ret.fill.na = 3,
                             TC = 30){

  lag.rank <- stats::lag(xts.rank, k = 1, na.pad = TRUE)
  z <- ret.fill.na

  lag.rank <- as.matrix(lag.rank)
  lag.rank[lag.rank > n] <- NA

  lag.rank[lag.rank <= n] <- 1

  mat.ret <- as.matrix(xts.ret) * lag.rank
  TCcost <- numeric(nrow(lag.rank))
  NTvec <- numeric(nrow(lag.rank))
  for(i in z:length(TCcost)) {
    NTvec[i] <- (num.assets - sum(as.numeric(lag.rank[i,] == lag.rank[(i-1),]), na.rm = TRUE))
    TCcost[i] <- (num.assets - sum(as.numeric(lag.rank[i,] == lag.rank[(i-1),]), na.rm = TRUE)) * (-1) * TC * 2
  }

  if(!is.null(TC)) {
    vec.returns <- rowSums(cbind(mat.ret, TCcost), na.rm = TRUE)
  } else {
    vec.returns <- rowSums(mat.ret, na.rm = TRUE)
  }

  vec.returns[1:z] <- NA

  vec.returns <- xts::xts(x = vec.returns, order.by = zoo::index(xts.ret))
  f <- list(mat = mat.ret, ret = vec.returns,
            rank = lag.rank, NumberTrades = sum(NTvec, na.rm = TRUE))
  return(f)
}
