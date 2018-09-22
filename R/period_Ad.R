#' Period Adjusted prices
#'
#' @param x xts object in the form OHLCAV
#' @param period Period at which one want to obtain data. Can be "days", "weeks", "months", "quarters" or "years"
#'
#' @return Adjusted prices
#' @export
#'
period_Ad <- function(x, period = c("days", "weeks", "months", "quarters", "years")){
  names <- sub("\\..*$", "", names(x)[1])
  quantmod::Ad(xts::to.period(x, indexAt = 'lastof', period = period,
                               drop.time = TRUE, name = names))
}
