#' fun_Shiny_PropAbs
#'
#' @description Returns plot for cumulated returns and weights, table of statistics and table of weights.
#' Designed to be used fr IPA's Shiny App
#'
#' @param Assets List of assets
#' @param ChoosenAssets Assets one want to look at
#' @param rf.asset Risk free rate
#' @param k Number of lookback periods
#' @param cf confidence interval
#' @param dateStart Start date of strategy
#' @param dateEnd End date of strategy
#' @param TCrate Transaction cost given rate
#' @param TCmin Transactin cost minimum amount
#' @param size Size of each transaction
#' @param nd Number of days in a year
#' @param alpha alpha value used for calculating VaR and ES
#' @param UseTrans Boolean if one want to use tranaction costs
#'
#' @return list
#'  \item{name weights}{Table of position of weights}
#'  \item{name table}{Table of statistics}
#'  \item{name cumplot}{Plot of cumulated returns}
#'  \item{name weightplot}{Plot of position of weights}
#' @export
#'
fun_Shiny_PropAbs <- function(Assets, ChoosenAssets = NULL, rf.asset = 1, k = 200,
         cf = 0.6, dateStart = NULL, dateEnd = NULL,
         TCrate = 0.001, TCmin = 29, size = 1,
         nd = 256, alpha = 0.05, UseTrans = TRUE) {

  Price_df <- do.call(cbind, lapply(Assets, quantmod::Ad))
  Price_df <- zoo::na.locf(Price_df)
  names(Price_df) <- sub("\\.[^.]*$", "", names(Price_df))

  dateRange = paste0(dateStart, "/", dateEnd)

  Price_df <- Price_df[dateRange,ChoosenAssets]

  x.mat <- as.matrix(Price_df)
  roc <- x.mat[-1,] / x.mat[-nrow(x.mat),] - 1

  ir <- sqrt(k) * SMAMatrixCpp(roc, k) / sdRollMatrixCpp(roc, k)
  momentum.p <- pt(ir, k - 1)

  weights <- weightsPropMatrix(momentum.p, cf = cf, k = k)

  dailyDiff <- diff(x.mat)
  StraReturn <- dailyDiff[-1,] * weights[- nrow(weights),] * size

  StraReturn[is.na(StraReturn)] <- 0

  colnames(weights) <- stringr::str_replace_all(names(Price_df), ".CO", "")
  rownames(weights) <- as.character(zoo::index(Price_df)[-1])

  NumberTrades <- numberTradesMatrixCpp(weights)

  if(UseTrans) {
  TransPrice <- getTransPrice(Price_df[-1,], NumberTrades * size)
  TransPrice[TransPrice == 0] <- NA
  TransPrice <- pmax(TransPrice*TCrate, TCmin)
  TransPrice[is.na(TransPrice)] <- 0
  CumReturn <- apply(StraReturn - TransPrice[-1,], 2, cumsum)
  } else {
    CumReturn <- apply(StraReturn, 2, cumsum)
  }

  colnames(CumReturn) <- stringr::str_replace_all(names(Price_df), ".CO", "")

  #Table
  Period <- paste(row.names(x.mat)[1], " to ", row.names(x.mat)[length(x.mat)])
  nOfYears <- as.numeric(as.Date(row.names(x.mat)[nrow(x.mat)])- as.Date(row.names(x.mat)[1]))/nd

  CGR <- (CumReturn[nrow(CumReturn),] + x.mat[1,]) / x.mat[1,]

  CAGR <- CGR / nOfYears

  vol <- apply(CumReturn, 2, sd, na.rm = TRUE)

  SharpeRatio <- (CGR - rf.asset) / vol

  VaR <- apply(StraReturn, 2, quantile, alpha, na.rm = TRUE)

  above <- StraReturn
  above[above > VaR] = NA
  ES <- apply(above, 2, mean, na.rm = TRUE)

  numberTrades <- colSums(NumberTrades, na.rm = TRUE)

  table <- data.frame(round(matrix(c(CGR, CAGR, vol, SharpeRatio,
                                     VaR, ES, numberTrades),
                                   nrow = 7, byrow = TRUE),
                            digits = 2))
  rownames(table) <- c("CGR",
                       "CAGR",
                       "vol",
                       "SharpeRatio",
                       "VaR",
                       "ES",
                       "#trades")
  colnames(table) <- stringr::str_replace_all(names(Price_df), ".CO", "")

  # Plot
  data.tidy <- data.frame(Date = rownames(CumReturn)[-c(1:k)], CumReturn[-c(1:k),]) %>%
    tidyr::gather(-Date, key = "Asset", value = "Return")

  data.tidy$Asset <- as.factor(data.tidy$Asset)
  data.tidy$Date <- lubridate::as_date(data.tidy$Date)

  if(ncol(CumReturn) < 9) {
    coul = RColorBrewer::brewer.pal(ncol(CumReturn),"Set1")
  } else {
    coul = RColorBrewer::brewer.pal(9,"Set1")
    coul = grDevices::colorRampPalette(coul)(ncol(CumReturn))
  }

  ### Plot data
  cumplot <- ggplot2::ggplot(data = data.tidy,
                             ggplot2::aes(x = Date, y = Return, color = Asset)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(title=paste0("Probability absoute momentum"),
                  y="Cumulated returns",
                  color= "Assets") +
    ggplot2::scale_color_manual(values = coul) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = (15), hjust = 0.5),
                   legend.title = ggplot2::element_text(colour = "steelblue", face = "bold.italic"),
                   legend.text = ggplot2::element_text(size = (10), colour = "steelblue4", face = "italic"),
                   axis.title = ggplot2::element_text(size = (10), colour = "steelblue4"),
                   panel.background = ggplot2::element_rect(fill = "lightblue",
                                                            colour = "lightblue",
                                                            size = 0.5, linetype = "solid"),
                   panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',
                                                            colour = "white"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',
                                                            colour = "white"))

  # Weight plot
  weight.tidy <- data.frame(Date = rownames(weights)[-c(1:k)], weights[-c(1:k),]) %>%
    tidyr::gather(-Date, key = "Asset", value = "Position")

  weight.tidy$Asset <- as.factor(weight.tidy$Asset)
  weight.tidy$Date <- lubridate::as_date(weight.tidy$Date)

  weightplot <- ggplot2::ggplot(data = weight.tidy,
                                ggplot2::aes(x = Date, y = Position, color = Asset)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::labs(title=paste0("Position of assets"),
                  y="Position",
                  color= "Assets") +
    ggplot2::scale_color_manual(values = coul) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = (15), hjust = 0.5),
                   legend.title = ggplot2::element_text(colour = "steelblue", face = "bold.italic"),
                   legend.text = ggplot2::element_text(size = (10), colour = "steelblue4", face = "italic"),
                   axis.title = ggplot2::element_text(size = (10), colour = "steelblue4"),
                   panel.background = ggplot2::element_rect(fill = "lightblue",
                                                            colour = "lightblue",
                                                            size = 0.5, linetype = "solid"),
                   panel.grid.major = ggplot2::element_line(size = 0.5, linetype = 'solid',
                                                            colour = "white"),
                   panel.grid.minor = ggplot2::element_line(size = 0.25, linetype = 'solid',
                                                            colour = "white"))

  out = list(weights = weights,
             table = table,
             cumplot = cumplot,
             weightplot = weightplot)
  return(out)
}
