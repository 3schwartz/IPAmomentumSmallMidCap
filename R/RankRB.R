#' Rank returns
#'
#'
#' @description Rank assets given thier return, ROC or other format the input is given in
#' @param x Returns in a dataframe or matrix.
#'
#' @return Object with input frame ranked
#' @export
#'
RankRB <- function(x){
  r <- xts::as.xts(t(apply(-x, 1, rank, na.last = "keep")))
  return(r)
}
