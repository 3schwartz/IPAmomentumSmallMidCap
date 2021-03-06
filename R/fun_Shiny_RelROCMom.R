#' Relative ROC based Momentum
#'
#' @description Returns a plot and statistics for a relative momentum based on ROC ranking
#' This function is customized to work with IPA's Shiny App
#'
#' @param listData Data from Yahoo in form OHLCAV. The adjusted prices is used s these must be present
#' @param period Period at which one want to obtain data. Can be "days", "weeks", "months", "quarters" or "years"
#' @param lookback Lookback periods used to rank assets
#' @param weights weights when using a weighted momentum. Vector which should have lenth three and sum to one
#' @param NumAssets Number of assets in optimal portfolio
#' @param dateStart Date at which the strategy shall start. The lookback period is looking before this date so it is the start date of the strategy.
#' @param size Number of asset one buy (or sell) in each transaction. Default is one
#' @param UseTC Logical if one want to use transaction cast
#' @param TCmin Minimum transactionamount
#' @param TCrate Rate in which the transaction cost is calculated. This is only used if it is over the minimal amount
#' @param dateEnd Date at which the strategy shall end
#' @param ChoosenAssets Assets one have choosen in strategy
#'
#' @return list
#'  \item{name plotCumReturn}{ggplot of cumreturns for different assets}
#'  \item{name StatTable}{Table of statistics}
#'  \item{name rank.first}{Table of rank for first lookback period}
#'  \item{name rank.second}{Table of rank for second lookback period}
#'  \item{name rank.third}{Table of rank for third lookback period}
#'  \item{name rank.ave}{Table of rank for average lookback period}
#'  \item{name rank.weight.ave}{Table of rank for weighted average lookback period}
#'
#' @export
fun_Shiny_RelROCMom <- function(listData, period = c("days", "weeks", "months", "quarters", "years"),
                                lookback = c(3, 6, 12),
                                dateStart = NULL, dateEnd = NULL,
                                weights = c(1/6, 2/3, 1/6),
                                NumAssets = 8, size = NULL,
                                UseTC = TRUE, TCmin = 30, TCrate = 0.001,
                                ChoosenAssets = NULL) {

  Price_df <- do.call(merge, lapply(listData, period_Ad, period = period))
  names(Price_df) <- sub("\\.[^.]*$", "", names(Price_df))
  Price_df <- zoo::na.locf(Price_df)

  Price_df <- Price_df[,stringr::str_replace_all(ChoosenAssets, ".CO", "")]
  # period returns
  # returns.roc <- TTR::ROC(x = Price_df, n = 1, type = "discrete", na.pad = TRUE)
  period.returns <- diff(Price_df)

  roc.first <- TTR::ROC(x = Price_df , n = lookback[1], type = "discrete")
  rank.first <- RankRB(roc.first)

  roc.second <- TTR::ROC(x = Price_df , n = lookback[2], type = "discrete")
  rank.second <- RankRB(roc.second)

  roc.third <- TTR::ROC(x = Price_df , n = lookback[3], type = "discrete")
  rank.third <- RankRB(roc.third)

  roc.ave <- weightedROC(x = Price_df, n = lookback,
                         weights = c(1/3, 1/3, 1/3))
  rank.ave <- RankRB(roc.ave)

  roc.weight.ave <- weightedROC(x = Price_df, n = lookback,
                                weights = weights)
  rank.weight.ave <- RankRB(roc.weight.ave)

  DateRange <- paste0(dateStart, "/", dateEnd)
  if(!is.null(DateRange)){
    period.returns <- period.returns[DateRange]
    rank.first <- rank.first[DateRange]
    rank.second <- rank.second[DateRange]
    rank.third <- rank.third[DateRange]
    rank.ave <- rank.ave[DateRange]
    rank.weight.ave <- rank.weight.ave[DateRange]
    Price_df <- Price_df[DateRange]
  }

  # run the backtest

  # momentum test based on month ROC to rank
  case1 <- RelativeMomentum(xts.ret = period.returns[lubridate::as_date(zoo::index(rank.first))],
                            xts.rank = rank.first,
                            size = size, price = Price_df,
                            UseTC = UseTC, TCmin = TCmin, TCrate = TCrate,
                            n = NumAssets, ret.fill.na = 3)

  case2 <- RelativeMomentum(xts.ret = period.returns[lubridate::as_date(zoo::index(rank.second))],
                            xts.rank = rank.second,
                            size = size, price = Price_df,
                            UseTC = UseTC, TCmin = TCmin, TCrate = TCrate,
                            n = NumAssets, ret.fill.na = 3)

  case3 <- RelativeMomentum(xts.ret = period.returns[lubridate::as_date(zoo::index(rank.third))],
                            xts.rank = rank.third,
                            size = size, price = Price_df,
                            UseTC = UseTC, TCmin = TCmin, TCrate = TCrate,
                            n = NumAssets, ret.fill.na = 3)

  case4 <- RelativeMomentum(xts.ret = period.returns[lubridate::as_date(zoo::index(rank.ave))],
                            xts.rank = rank.ave,
                            size = size, price = Price_df,
                            UseTC = UseTC, TCmin = TCmin, TCrate = TCrate,
                            n = NumAssets, ret.fill.na = 3)

  case5 <- RelativeMomentum(xts.ret = period.returns[lubridate::as_date(zoo::index(rank.weight.ave))],
                            xts.rank = rank.weight.ave,
                            size = size, price = Price_df,
                            UseTC = UseTC, TCmin = TCmin, TCrate = TCrate,
                            n = NumAssets, ret.fill.na = 3)

  returns <- cbind(case1$ret, case2$ret, case3$ret, case4$ret, case5$ret)
  colnames(returns) <- c(paste0(lookback[1], "-", period),
                         paste0(lookback[2], "-", period),
                         paste0(lookback[3], "-", period),
                         "Ave",
                         "Weighted Ave")

  cumsum_df <- data.frame(cumsum(returns[-unique(which(is.na(returns), arr.ind = TRUE)[,1]),]))

  cumsum_df <- data.frame(as.Date(zoo::index(returns[-unique(which(is.na(returns), arr.ind = TRUE)[,1]),])),
                          cumsum_df, row.names = NULL)
  colnames(cumsum_df) <- c("Date", colnames(returns))

  cumsum_tb <-  tidyr::gather(cumsum_df, -"Date", key = "Strategy", value = "Return")

  plotCumReturn <- ggplot2::ggplot(cumsum_tb, ggplot2::aes(x= Date, y= Return, col= Strategy)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(title=paste0("ROC Adjusted Momentum: Top ", NumAssets, " assets"),
                  y="Returns",
                  color= "Strategy") +
    ggplot2::scale_color_manual(values = c(RColorBrewer::brewer.pal(ncol(returns),"Set1"))) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = (15), hjust = 0.5),
                   legend.title = ggplot2::element_text(colour = "steelblue", face = "bold.italic"),
                   legend.text = ggplot2::element_text(size = (10), colour = "steelblue4", face = "italic"),
                   axis.title = ggplot2::element_text(size = (10), colour = "steelblue4"),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5, size = 8),
                   panel.background = ggplot2::element_rect(fill = "lightblue",
                                                            colour = "lightblue",
                                                            size = 0.5, linetype = "solid"),
                   panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',
                                                            colour = "white"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',
                                                            colour = "white"))

  NumberOfTrades <- cbind(case1$NumberTrades, case2$NumberTrades,
                          case3$NumberTrades, case4$NumberTrades,
                          case5$NumberTrades)
  rownames(NumberOfTrades) = "NumberOfTrades"

  StatTable <- t(cbind(t(PerformanceAnalytics::table.Stats(returns)),
                       NumberOfTrades = t(NumberOfTrades)))

  out <- list(plotCumReturn = plotCumReturn,
              StatTable = StatTable,
              rank.first = rank.first,
              rank.second = rank.second,
              rank.third = rank.third,
              rank.ave = rank.ave,
              rank.weight.ave = rank.weight.ave)


  return(out)
}
