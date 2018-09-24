#' Error adjusted moving average
#'
#' @param Price_Daily xts object in form of a dataframe with daily prices
#' @param SMAperiod Period in which one want to adjust prices. Hence this period is used when calculating forecast and thereby errors
#' @param Lag Number of periods used when calculating simple momentum of the roc/errors
#' @param period Periods at which one is rebalancing. Can be "days", "weeks", "months", "quarters" or "years"
#' @param CompareObj Object of which final object should be compare to. Ex. if period is monthly, then a frame with the wanted index should be provided
#'
#' @return Matrix of simple moving average o
#' @export
#'
ErrMov <- function(Price_Daily, SMAperiod = 10, Lag = 3, CompareObj = NULL,
                   period = c("days", "weeks", "months", "quarters", "years")) {
  Price_Daily.mat <- as.matrix(Price_Daily)
  roc <- TTR::ROC(x = Price_Daily.mat, n = 1, type = "discrete", na.pad = TRUE)
  # roc <- x.mat[-1] / x.mat[-length(x.mat)] - 1

  forecast <- SMAMatrixCpp(roc, SMAperiod)
  error <- roc[-1,] - forecast[-nrow(forecast),]
  mae <- SMAMatrixCpp(abs(error), SMAperiod)

  ErrDay <- roc[-1,]/mae

  ErrDay.xts <- xts::xts(ErrDay, order.by = zoo::index(Price_Daily)[-1])

  x <- as.list(ErrDay.xts)

  ErrPeriod <- zoo::na.locf(do.call(cbind, lapply(seq_along(x), function(i) {
    # names <- sub("\\..*$", "", names(x)[1])
    names <- names(x)[[i]]
    quantmod::Cl(xts::to.period(x[[i]], indexAt = 'lastof', period = period,
                                drop.time = TRUE, name = names))
  })))
  # names(ErrPeriod) <- sub("\\.[^.]*$", "", names(ErrPeriod))

  SMAErrPeriod <- SMAMatrixCpp(ErrPeriod, Lag)
  colnames(SMAErrPeriod) <- sub("\\.[^.]*$", "", names(ErrPeriod))

  if(!is.null(CompareObj)) {
    diff <- nrow(CompareObj) - nrow(SMAErrPeriod)
    if(diff > 0) {
      NAs <- matrix(rep(rep(NA, ncol(SMAErrPeriod)), diff), ncol = diff)
      SMAErrPeriod <- t(cbind(NAs, t(SMAErrPeriod)))
      SMAErrPeriod.xts <- xts::xts(SMAErrPeriod, order.by = zoo::index(CompareObj))
    } else {
      SMAErrPeriod.xts <- xts::xts(SMAErrPeriod, order.by = zoo::index(ErrPeriod))
      SMAErrPeriod.xts <- SMAErrPeriod.xts[zoo::index(CompareObj)[zoo::index(CompareObj) %in% zoo::index(ErrPeriod)]]
    }
  } else {
    SMAErrPeriod.xts <- xts::xts(SMAErrPeriod, order.by = zoo::index(ErrPeriod))
  }

  return(SMAErrPeriod.xts)
}
