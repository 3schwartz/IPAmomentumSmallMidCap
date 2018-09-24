#' Relative Momentum Strategy
#'
#' @param xts.ret Object of returns
#' @param xts.rank Object of ranked returns
#' @param n Number of assets in portfolio. Has to be less than number of assets
#' @param ret.fill.na Number of NA'sto put in the beginning
#' @param size Size of long position when one is buying. Increasing this can decrease the size of transaction cost, assuming they are independent of size
#' @param price object of prices
#' @param UseTC Logical if one want to use transaction cast
#' @param TCmin Minimum transactionamount
#' @param TCrate Rate in which the transaction cost is calculated. This is only used if it is over the minimal amount
#'
#' @return  list
#'  \item{name mat}{Matrix of returns for top ranked assets}
#'  \item{name ret}{Vector of returns for tp ranked assets. If TC is not null, tranding cost is in the calculation}
#'  \item{name rank}{Matrix of ranks for the top}
#'  \item{name NumberTrades}{Scalar number of trades}
#' @export
#'
RelativeMomentum <- function(xts.ret, xts.rank, n = 1, ret.fill.na = 3,
                             UseTC = TRUE, TCmin = 30, TCrate = 0.001,
                             size = 1, price){

  lag.rank <- stats::lag(xts.rank, k = 1, na.pad = TRUE)
  z <- ret.fill.na

  lag.rank <- as.matrix(lag.rank)
  lag.rank[lag.rank > n] <- NA

  lag.rank[lag.rank <= n] <- 1

  mat.ret <- as.matrix(xts.ret) * lag.rank

  TCcost <- numeric(nrow(lag.rank))
  NTvec <- numeric(nrow(lag.rank))

  for(i in z:length(TCcost)) {
    NTvec[i] <- sum(
      as.numeric(is.na(lag.rank[i-1,]) & lag.rank[i,] == 1 | lag.rank[i-1,] == 1 & is.na(lag.rank[i,]))
      , na.rm = TRUE)
    ind <- as.numeric(which(is.na(lag.rank[i-1,]) & lag.rank[i,] == 1 | lag.rank[i-1,] == 1 & is.na(lag.rank[i,])))
    if(length(ind) == 0) {
      TCcost[i] <- 0
    } else {
      TCcost[i] <- -sum(pmax(price[i,ind]*size*TCrate, TCmin, na.rm = TRUE),na.rm = TRUE)
    }
  }

  if(UseTC) {
    vec.returns <- rowSums(cbind(mat.ret * size, TCcost), na.rm = TRUE)
  } else {
    vec.returns <- rowSums(mat.ret * size, na.rm = TRUE)
  }

  vec.returns[1:z] <- NA

  vec.returns <- xts::xts(x = vec.returns, order.by = zoo::index(xts.ret))
  f <- list(mat = mat.ret, ret = vec.returns,
            rank = lag.rank, NumberTrades = sum(NTvec, na.rm = TRUE))
  return(f)
}
