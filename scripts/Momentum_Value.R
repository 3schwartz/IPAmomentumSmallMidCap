library(IPAMomentum)
library(dplyr)
library(ggplot2)
library(gganimate)

#### Data #####
### Mid cap
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

### Small Cap
SmallCapTicker <- c(
  "Admiral Capital A/S B" = "ADMCAP-B.CO",
  "AGF B" = "AGF-B.CO",
  "Andersen & Martini B A/S" = "AM-B.CO",
  "Arkil Holding B A/S" = "ARKIL-B.CO",
  # "Athena Investments" = "G3E.F", (currency in euro)
  "Atlantic Petroleum P/F" = "ATLA-DKK.CO",
  "BioPorto A/S" = "BIOPOR.CO",
  "Blue Vision A/S A" = "BLVIS-A.CO",
  #"Boliga Gruppen A/S" = "EI.CO",
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
  "German High Street Properties" = "GERHSP.CO",
  "Glunz & Jensen Holding A/S" = "GJ.CO",
  "Grønlandsbanken A/S" = "GRLA.CO",
  "Gyldendal A A/S" = "GYLD-A.CO",
  "Gyldendal B A/S" = "GYLD-B.CO",
  "H+H International A/S" = "HH.CO",
  "Harboes Bryggeri B A/S" = "HARB-B.CO",
  "Hvidbjerg Bank A/S" = "HVID.CO",
  #"InterMail B A/S" = "IMAIL-B.CO",
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
#####

MidAssets <- lapply(MidCapTicker, quantmod::getSymbols, auto.assign = FALSE)
SmallAssets <- lapply(SmallCapTicker, quantmod::getSymbols, auto.assign = FALSE)
BothAssets <- c(MidAssets, SmallAssets)

Price_df <- do.call(merge, lapply(BothAssets, period_Ad, period = "months"))
names(Price_df) <- sub("\\.Adjusted$", "", names(Price_df))
# sub("\\.[^.]*$", "", names(BothAssets))

Prices2015_2017 <- Price_df["2015-01-31/2017-12-31"]

### Stocks with more than 10 % NA's
colnames(Prices2015_2017)[apply(apply(Prices2015_2017, 2, is.na), 2, mean) >= 0.1]
colnames(Prices2015_2017)[!apply(apply(Prices2015_2017, 2, is.na), 2, mean) >= 0.1]

#Prices2015_2017[,colnames(Prices2015_2017)[apply(apply(Prices2015_2017, 2, is.na), 2, mean) >= 0.1]]

Prices <- Prices2015_2017[,colnames(Prices2015_2017)[!apply(apply(Prices2015_2017, 2, is.na), 2, mean) >= 0.1]]
rtPrices <- diff(log(Prices))

Prices_lag2 <- xts::lag.xts(Prices, k = 2)
Prices_lag12 <- xts::lag.xts(Prices, k = 12)

rt <- log(Prices_lag2) - log(Prices_lag12)
rt <- rt[rowSums(is.na(rt)) != ncol(rt), ]
rank_rt <- t(apply(-rt, 1, rank))

n <- 10
initial <- 100000
get_return_from_rank <- function(rank_rt = rank_rt, Prices = Prices,
                                 n = 10, initial = 100000) {
lag.rank <- xts::lag.xts(rank_rt, k = 1)

n <- n
lag.rank <- as.matrix(lag.rank)
lag.rank[lag.rank > n] <- NA
lag.rank[lag.rank <= n] <- 1


roc <- TTR::ROC(x = Prices , n = 1, type = "discrete")[rownames(lag.rank),]
out <- matrix(, nrow = nrow(roc), ncol = ncol(roc))

foo <- as.matrix(0.1 * (1 + roc) * lag.rank)

initial <- initial
porte <- numeric(nrow(out))
porte[1] <- initial

out[2, ] <- foo[2, ] * initial
porte[2] <- sum(out[2, ], na.rm = TRUE)

for (i in 3:nrow(out)) {
  out[i, ] <-foo[i, ] * porte[i-1]
  porte[i] <- sum(out[i, ], na.rm = TRUE)
  print(porte[i])
}
porte_xts <- xts::xts(porte, order.by = lubridate::as_date(rownames(foo)))
return(porte_xts)
}
porte_xts <- get_return_from_rank()

# readr::write_csv(data.frame(Prices), path = "C:/Users/sch/Documents/prices.csv")


All <- as_tibble(porte_xts) %>%
  transmute(Date = as.character(zoo::index(porte_xts)),
            Momentum = as.matrix(porte_xts[,1]))

sharpe <- All %>%
  select(-Date) %>%
  apply(2, tseries::sharpe, scale = sqrt(24))
SD <- All %>%
  select(-Date) %>%
  apply(2, function(x) {sd(x) * sqrt(250)})

stats <- cbind(sharpe, SD) %>%
  t() %>%
  as_tibble()

All.tidy <- tidyr::gather(All, -Date, key = "Strategy", value = "Return")
All.tidy$Date <- lubridate::as_date(All.tidy$Date)

listAll <- list()
for(i in 1:nrow(All)){
  listAll[[i]] <- All[1:i,] %>%
    mutate(Frame = i)
}

All.tidy.frame <- tidyr::gather(do.call(bind_rows, listAll),
                                -Date, -Frame, key = "Strategy", value = "Return")
All.tidy.frame$Date <- lubridate::as_date(All.tidy.frame$Date)


ggplot(All.tidy, aes(x = Date, y = Return, color = Strategy)) +
  geom_line(size = 0.85) +
  ggtitle("Backtest of financial strategies") +
  scale_color_manual(values = c(RColorBrewer::brewer.pal(length(unique(All.tidy$Strategy)),
                                                         "Set1"))) +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"))

ggsave(filename = "animate.pdf", path = "./output/",
       width = 8, height = 4, device = "pdf")

p <- ggplot(All.tidy.frame, aes(x = Date, y = Return, color = Strategy)) +
  geom_line(size = 0.85) +
  ggtitle("Backtest of financial strategies") +
  scale_color_manual(values = c(RColorBrewer::brewer.pal(length(unique(All.tidy.frame$Strategy)),
                                                         "Set1")[1])) +
  transition_manual(Frame) +
  # ease_aes('linear') +
  theme_classic() +
  theme(plot.title = element_text(size = 20, face = "bold"))

animate(p, fps = 10, renderer = gifski_renderer(loop = FALSE),
        width = 800, height = 400)

anim_save("./output/animation.gif")
