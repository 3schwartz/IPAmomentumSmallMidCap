#' Monthly Adjusted prices
#'
#' @param x xts object in the form OHLCAV
#'
#' @return Adjusted prices
#' @export
#'
month_Ad <- function(x){
  names <- sub("\\..*$", "", names(x)[1])
  quantmod::Ad(xts::to.monthly(x, indexAt = 'lastof',
                          drop.time = TRUE, name = names))
}
