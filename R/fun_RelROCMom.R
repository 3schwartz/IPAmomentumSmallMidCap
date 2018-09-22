#' Relative ROC based Momentum
#'
#' @description Returns a plot and statistics for a relative momentum based on ROC ranking
#'
#' @param listData Data from Yahoo in form OHLCAV. The adjusted prices is used s these must be present
#' @param period Period at which one want to obtain data. Can be "days", "weeks", "months", "quarters" or "years"
#' @param lookback Lookback periods used to rank assets
#' @param weights weights when using a weighted momentum. Vector which should have lenth three and sum to one
#' @param num.assets Number of assets in optimal portfolio
#' @param savePlot Outdir if one want to save plot
#' @param name If savePlot !is.null then the name (OBS: also _MOM_period is appended)
#' @param width If savePlot !is.null then the width of plot
#' @param height If savePlot !is.null then the height of plot
#' @param TC Tranding costs. If null, then vector without trading cost will be returned
#' @param dateStart Date at which the strategy shall start. The lookback period is looking before this date so it is the start date of the strategy. The form should be ex: "2018/", "2018-01/" or "2018-01-01/"
#'
#' @return list
#'  \item{name plotCumReturn}{ggplot of cumreturns for different assets}
#'  \item{name StatTable}{Table of statistics}
#'  \item{name xtable_StatTable}{LateX code for above table of statistics}
#' @export
fun_RelROCMom <- function(listData, period = c("days", "weeks", "months", "quarters", "years"),
                          lookback = c(3, 6, 12), dateStart = NULL,
                          weights = c(1/6, 2/3, 1/6),
                          num.assets = 8, TC = NULL,
                          savePlot = NULL,
                          name = NULL, width = 8, height = 6) {
  Price_df <- do.call(merge, lapply(listData, period_Ad, period = period))
  names(Price_df) <- sub("\\.[^.]*$", "", names(Price_df))

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

  if(!is.null(dateStart)){
    period.returns <- period.returns[dateStart]
    rank.first <- rank.first[dateStart]
    rank.second <- rank.second[dateStart]
    rank.third <- rank.third[dateStart]
    rank.ave <- rank.ave[dateStart]
    rank.weight.ave <- rank.weight.ave[dateStart]
  }

  # run the backtest
  num.assets <- num.assets

  # momentum test based on month ROC to rank
  case1 <- RelativeMomentum(xts.ret = period.returns[lubridate::as_date(zoo::index(rank.first))],
                            xts.rank = rank.first, TC = TC,
                            n = num.assets, ret.fill.na = 3)

  case2 <- RelativeMomentum(xts.ret = period.returns[lubridate::as_date(zoo::index(rank.second))],
                            xts.rank = rank.second, TC = TC,
                            n = num.assets, ret.fill.na = 3)

  case3 <- RelativeMomentum(xts.ret = period.returns[lubridate::as_date(zoo::index(rank.third))],
                            xts.rank = rank.third, TC = TC,
                            n = num.assets, ret.fill.na = 3)

  case4 <- RelativeMomentum(xts.ret = period.returns[lubridate::as_date(zoo::index(rank.ave))],
                            xts.rank = rank.ave, TC = TC,
                            n = num.assets, ret.fill.na = 3)

  case5 <- RelativeMomentum(xts.ret = period.returns[lubridate::as_date(zoo::index(rank.weight.ave))],
                            xts.rank = rank.weight.ave, TC = TC,
                            n = num.assets, ret.fill.na = 3)

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
    ggplot2::labs(title=paste0("Momentum Cumulative Return: Top ", num.assets, " assets"),
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
                       CAGR =  CAGR(returns, m = 12),
                       max.dd = t(PerformanceAnalytics::maxDrawdown(returns)),
                       NumberOfTrades = t(NumberOfTrades)))

  xtable_StatTable <- xtable::xtable(StatTable)

  out <- list(plotCumReturn = plotCumReturn,
              StatTable = StatTable,
              xtable_StatTable = xtable_StatTable)

  if(!is.null(savePlot)) {
    pdf(file = paste0(savePlot, "/", name,"_Mom_",period, ".pdf"),
        width = width, height = height)
    print(plotCumReturn)
    dev.off()
  }

  return(out)
}
