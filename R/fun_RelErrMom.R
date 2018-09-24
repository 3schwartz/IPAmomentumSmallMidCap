#' Relative Error Adjusted based Momentum
#'
#' @description Returns a plot and statistics for a relative momentum based on error adjusted ranking
#' OBS! Objects with only NA's values will be removed under calculations
#'
#' @param listData Data from Yahoo in form OHLCAV. The adjusted prices is used s these must be present
#' @param period Period at which one want to obtain data. Can be "days", "weeks", "months", "quarters" or "years"
#' @param lookback Lookback periods used to rank assets
#' @param weights weights when using a weighted momentum. Vector which should have lenth three and sum to one
#' @param NumAssets Number of assets in optimal portfolio
#' @param savePlot Outdir if one want to save plot
#' @param name Used on graph. If savePlot !is.null then the name (OBS: also _MOM_period is appended)
#' @param width If savePlot !is.null then the width of plot
#' @param height If savePlot !is.null then the height of plot
#' @param dateStart Date at which the strategy shall start. The lookback period is looking before this date so it is the start date of the strategy. The form should be ex: "2018/", "2018-01/" or "2018-01-01/"
#' @param size Number of asset one buy (or sell) in each transaction. Default is one
#' @param UseTC Logical if one want to use transaction cast
#' @param TCmin Minimum transactionamount
#' @param TCrate Rate in which the transaction cost is calculated. This is only used if it is over the minimal amount
#' @param SMAperiod int Period one is using to calculate errors. These are calcualted on daily frequency and not the period specified
#'
#' @return list
#'  \item{name plotCumReturn}{ggplot of cumreturns for different assets}
#'  \item{name StatTable}{Table of statistics}
#'  \item{name xtable_StatTable}{LateX code for above table of statistics}
#' @export
fun_RelErrMom <- function(listData, period = c("days", "weeks", "months", "quarters", "years"),
                          lookback = c(3, 6, 12), dateStart = NULL,
                          weights = c(1/6, 2/3, 1/6),
                          NumAssets = 8, size = NULL,
                          UseTC = TRUE, TCmin = 30, TCrate = 0.001,
                          savePlot = NULL,
                          name = NULL, width = 8, height = 6,
                          SMAperiod = 10) {
  Price_df <- do.call(merge, lapply(listData, period_Ad, period = period))
  Price_df_day <- do.call(merge, lapply(listData, function(x) {
    names <- sub("\\..*$", "", names(x)[1])
    quantmod::Ad(xts::to.period(x, period = "days",
                                drop.time = TRUE))
  }))
  names(Price_df_day) <- sub("\\.[^.]*$", "", names(Price_df))
  names(Price_df) <- sub("\\.[^.]*$", "", names(Price_df))

  # period returns
  # returns.roc <- TTR::ROC(x = Price_df, n = 1, type = "discrete", na.pad = TRUE)
  period.returns <- diff(Price_df)

  Err.first <- ErrMov(Price_df_day, SMAperiod = SMAperiod,
                      Lag = lookback[1], period = period, CompareObj = Price_df)
  rank.first <- RankRB(Err.first)

  Err.second <- ErrMov(Price_df_day, SMAperiod = SMAperiod,
                       Lag = lookback[2], period = period, CompareObj = Price_df)
  rank.second <- RankRB(Err.second)

  Err.third <- ErrMov(Price_df_day, SMAperiod = SMAperiod,
                      Lag = lookback[3], period = period, CompareObj = Price_df)
  rank.third <- RankRB(Err.third)

  Err.ave <- (Err.first * (1/3) + Err.second * (1/3) + Err.third * (1/3)) / sum(weights)
  rank.ave <- RankRB(Err.ave)

  Err.weight.ave <- (Err.first * weights[1] + Err.second * weights[2] + Err.third * weights[3]) / sum(weights)
  rank.weight.ave <- RankRB(Err.weight.ave)

  if(!is.null(dateStart)){
    period.returns <- period.returns[dateStart]
    rank.first <- rank.first[dateStart]
    rank.second <- rank.second[dateStart]
    rank.third <- rank.third[dateStart]
    rank.ave <- rank.ave[dateStart]
    rank.weight.ave <- rank.weight.ave[dateStart]
    Price_df <- Price_df[dateStart]
  }

  # run the backtest

  # momentum test based on month ROC to rank
  case1 <- RelativeMomentum(xts.ret = period.returns[lubridate::as_date(zoo::index(rank.first)),
                                                     names(rank.first)],
                            xts.rank = rank.first,
                            size = size, price = Price_df,
                            UseTC = UseTC, TCmin = TCmin, TCrate = TCrate,
                            n = NumAssets, ret.fill.na = 1)

  case2 <- RelativeMomentum(xts.ret = period.returns[lubridate::as_date(zoo::index(rank.second)),
                                                     names(rank.second)],
                            xts.rank = rank.second,
                            size = size, price = Price_df,
                            UseTC = UseTC, TCmin = TCmin, TCrate = TCrate,
                            n = NumAssets, ret.fill.na = 1)

  case3 <- RelativeMomentum(xts.ret = period.returns[lubridate::as_date(zoo::index(rank.third)),
                                                     names(rank.third)],
                            xts.rank = rank.third,
                            size = size, price = Price_df,
                            UseTC = UseTC, TCmin = TCmin, TCrate = TCrate,
                            n = NumAssets, ret.fill.na = 1)

  case4 <- RelativeMomentum(xts.ret = period.returns[lubridate::as_date(zoo::index(rank.ave)),
                                                     names(rank.ave)],
                            xts.rank = rank.ave,
                            size = size, price = Price_df,
                            UseTC = UseTC, TCmin = TCmin, TCrate = TCrate,
                            n = NumAssets, ret.fill.na = 1)

  case5 <- RelativeMomentum(xts.ret = period.returns[lubridate::as_date(zoo::index(rank.weight.ave)),
                                                     names(rank.weight.ave)],
                            xts.rank = rank.weight.ave,
                            size = size, price = Price_df,
                            UseTC = UseTC, TCmin = TCmin, TCrate = TCrate,
                            n = NumAssets, ret.fill.na = 1)

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
    ggplot2::labs(title=paste0("Error Adjussted Momentum: Top ", NumAssets, " assets \n", name),
                  y="Cumulative Return",
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

  # StatTable <- t(cbind(t(PerformanceAnalytics::table.Stats(returns)),
  #                      CAGR =  CAGR(returns, m = 12),
  #                      max.dd = t(PerformanceAnalytics::maxDrawdown(returns)),
  #                      NumberOfTrades = t(NumberOfTrades)))

  StatTable <- t(cbind(t(PerformanceAnalytics::table.Stats(returns)),
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
