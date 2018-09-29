SmallCapTicker <- c(
  "Admiral Capital A/S B" = "ADMCAP-B.CO",
  "AGF B" = "AGF-B.CO",
  "Andersen & Martini B A/S" = "AM-B.CO",
  "Arkil Holding B A/S" = "ARKIL-B.CO",
  # "Athena Investments" = "G3E.F", (currency in euro)
  "Atlantic Petroleum P/F" = "ATLA-DKK.CO",
  "BioPorto A/S" = "BIOPOR.CO",
  "Blue Vision A/S A" = "BLVIS-A.CO",
  "Boliga Gruppen A/S" = "EI.CO",
  "Brd Klee B A/S" = "KLEE-B.CO",
  "Brdr A & O Johansen" = "AOJ-P.CO",
  "Brøndby IF Fodbold A/S" = "BIF.CO",
  "cBrain A/S" = "CBRAIN.CO",
  "Cemat A/S" = "CEMAT.CO",
  "ChemoMetec A/S" = "CHEMM.CO",
  "Copenhagen Capital A/S Stam" = "CPHCAP-ST.CO",
  "Danske Andelskassers Bank A/S" = "DAB.CO",
  "Dantax A/S" = "DANT.CO",
  "Djurslands Bank A/S" = "DJUR.CO",
  "DLH A/S" = "DLH.CO",
  "Egetæpper B A/S" = "EGE-B.CO",
  "F.E. Bording B A/S" = "BORD-B.CO",
  "Fast Ejendom Danmark A/S" = "FED.CO",
  "FirstFarms A/S" = "FFARMS.CO",
  "Flügger B A/S" = "FLUG-B.CO",
  "Fynske Bank A/S" = "FYNBK.CO",
  "Gabriel Holding A/S" = "GABR.CO",
  # "German High Street Properties" = "GERHSP-B.CO",
  "Glunz & Jensen Holding A/S" = "GJ.CO",
  "Grønlandsbanken A/S" = "GRLA.CO",
  "Gyldendal A A/S" = "GYLD-A.CO",
  "Gyldendal B A/S" = "GYLD-B.CO",
  "H+H International A/S" = "HH.CO",
  "Harboes Bryggeri B A/S" = "HARB-B.CO",
  "Hvidbjerg Bank A/S" = "HVID.CO",
  # "InterMail B A/S" = "IMAIL-B.CO",
  "Kreditbanken A/S" = "KRE.CO",
  "Lollands Bank A/S" = "LOLB.CO",
  "Luxor B A/S" = "LUXOR-B.CO",
  "Lån og Spar Bank A/S" = "LASP.CO",
  "Migatronic B A/S" = "MIGA-B.CO",
  "Monberg & Thorsen B A/S" = "MT-B.CO",
  "Møns Bank A/S" = "MNBA.CO",
  "NeuroSearch A/S" = "NEUR.CO",
  "Newcap Holding A/S" = "NEWCAP.CO",
  "Nordfyns Bank A/S" = "NRDF.CO",
  "Nordic Shipholding A/S" = "NORDIC.CO",
  "North Media A/S" = "NORTHM.CO",
  "NTR Holding B A/S"  = "NTR-B.CO",
  "Onxeo SA" = "ONXEO.CO",
  "Park Street Nordicom A/S A" = "PSNRDC-A.CO",
  "Parken Sport & Entertainme"= "PARKEN.CO",
  "Prime Office A/S" = "PRIMOF.CO",
  "Rias B A/S" = "RIAS-B.CO",
  "Roblon B A/S" = "RBLN-B.CO",
  "Rovsing A/S" = "ROV.CO",
  "RTX A/S" = "RTX.CO",
  "Salling Bank A/S" = "SALB.CO",
  "Sanistål A/S" = "SANI.CO",
  "Santa Fe Group A/S" = "SFG.CO",
  "Scandinavian Brake Systems A/S" = "SBS.CO",
  "Scandinavian Private Equity" = "SPEAS.CO",
  "Silkeborg IF Invest A/S" = "SIF.CO",
  "Skako A/S" = "SKAKO.CO",
  "Skjern Bank A/S" = "SKJE.CO",
  "Small Cap Danmark A/S" = "SCD.CO",
  "Strategic Investments A/S" = "STRINV.CO",
  "TCM Group A/S" = "TCM.CO",
  "TK Development A/S" = "TKDV.CO",
  "Totalbanken A/S" = "TOTA.CO",
  "Victoria Properties A/S" = "VIPRO.CO",
  "Aalborg Boldspilklub A/S" = "AAB.CO")

MidCapTicker <- c(
  "Bang & Olufsen Holding A/S" = "BO.CO",
  "BankNordik P/F" = "BNORDIK-CSE.CO",
  "Bavarian Nordic A/S" = "BAVA.CO",
  "Brdr.Hartmann A/S" = "HART.CO",
  "Columbus A/S" = "COLUM.CO",
  "D/S Norden" = "DNORD.CO",
  "Højgaard Holding A A/S" = "HOEJ-A.CO",
  "Højgaard Holding B A/S" = "HOEJ-B.CO",
  "IC Group A/S" = "IC.CO",
  "Jutlander Bank A/S" = "JUTBK.CO",
  "Matas A/S" = "MATAS.CO",
  "Nnit A/S" = "NNIT.CO",
  "Orphazyme" = "ORPHA.CO",
  "Per Aarsleff Holding A/S B"= "PAAL-B.CO",
  "Ringkjøbing Landbobank A/S" = "RILBA.CO",
  "SAS AB" = "SAS-DKK.CO",
  "Solar B A/S" = "SOLAR-B.CO",
  "SP Group A/S" = "SPG.CO",
  "Sparekassen Sjælland-Fyn A/S" = "SPKSJF.CO",
  "Tivoli A/S" = "TIV.CO",
  "Torm PLC A" = "TRMD-A.CO",
  "United International Enterpris" = "UIE.CO",
  "Veloxis Pharmaceuticals A/S" = "VELO.CO",
  "Vestjysk Bank A/S" = "VJBA.CO",
  "Zealand Pharma A/S" = "ZEAL.CO")

Assets <- lapply(c(MidCapTicker, SmallCapTicker), quantmod::getSymbols, auto.assign = FALSE)

nameSmall <- names(SmallCapTicker)
SmallCapTicker <- stringr::str_replace_all(SmallCapTicker, "-", ".")
names(SmallCapTicker) <- nameSmall

nameMid <- names(MidCapTicker)
MidCapTicker <- stringr::str_replace_all(MidCapTicker, "-", ".")
names(MidCapTicker) <- nameMid

ChoosenAssets <- c(MidCapTicker, SmallCapTicker)


function(listData, period = c("days", "weeks", "months", "quarters", "years"),
         lookback = c(3, 6, 12),
         dateStart = NULL, dateEnd = NULL,
         weights = c(1/6, 2/3, 1/6),
         NumAssets = 8, size = NULL,
         UseTC = TRUE, TCmin = 30, TCrate = 0.001,
         SMAperiod = 10,
         ChoosenAssets = NULL) {

  function(x, k = 200, nd = 365.25, rf = 0, alpha = 0.05,
           Initpocket = NULL, TransCost = NULL,
           confidence.level = 0.60,
           rf.asset = 0, size = 1) {

    function(Assets, ChoosenAssets = NULL, rf.asset = 1, k = 200,
             cf = 0.6, dateStart = NULL, dateEnd = NULL,
             TCrate = 0.001, TCmin = 29, size = 1,
             nd = 365.25, alpha = 0.05) {

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
      TransPrice <- getTransPrice(Price_df[-1,], NumberTrades * size)
      TransPrice[TransPrice == 0] <- NA
      TransPrice <- pmax(TransPrice*TCrate, TCmin)
      TransPrice[is.na(TransPrice)] <- 0

      CumReturn <- apply(StraReturn - TransPrice[-1,], 2, cumsum)
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

      weightplot <- ggplot2::ggplot(data = data.tidy,
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

}
