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
  "German High Street Properties" = "GERHSP-B.CO",
  "Glunz & Jensen Holding A/S" = "GJ.CO",
  "Grønlandsbanken A/S" = "GRLA.CO",
  "Gyldendal A A/S" = "GYLD-A.CO",
  "Gyldendal B A/S" = "GYLD-B.CO",
  "H+H International A/S" = "HH.CO",
  "Harboes Bryggeri B A/S" = "HARB-B.CO",
  "Hvidbjerg Bank A/S" = "HVID.CO",
  "InterMail B A/S" = "IMAIL-B.CO",
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

#### Without ####
library(IPAMomentumData)
BothWithout <- fun_RelROCMom(BothAssets, period = "months",
                             lookback = c(3, 6, 12), dateStart = NULL,
                             weights = c(1/6, 2/3, 1/6),
                             NumAssets = 15, size = 1,
                             UseTC = FALSE, TCmin = 0, TCrate = 0,
                             savePlot = NULL,
                             name = "Both-Without", width = 8, height = 6)
BothWithout2010 <- fun_RelROCMom(BothAssets, period = "months",
                                 lookback = c(3, 6, 12), dateStart = "2010/",
                                 weights = c(1/6, 2/3, 1/6),
                                 NumAssets = 15, size = 1,
                                 UseTC = FALSE, TCmin = 0, TCrate = 0,
                                 savePlot = NULL,
                                 name = "Both-Without-2010", width = 8, height = 6)
BothWithout2017 <- fun_RelROCMom(BothAssets, period = "months",
                                 lookback = c(3, 6, 12), dateStart = "2017/",
                                 weights = c(1/6, 2/3, 1/6),
                                 NumAssets = 15, size = 1,
                                 UseTC = FALSE, TCmin = 0, TCrate = 0,
                                 savePlot = NULL,
                                 name = "Both-Without-2017", width = 8, height = 6)
#### Opdelt ####
SmallWithout2010 <- fun_RelROCMom(SmallAssets, period = "months",
                                 lookback = c(3, 6, 12), dateStart = "2010/",
                                 weights = c(1/6, 2/3, 1/6),
                                 NumAssets = 10, size = 1,
                                 UseTC = FALSE, TCmin = 0, TCrate = 0,
                                 savePlot = NULL,
                                 name = "Small-Without-2010", width = 8, height = 6)
MidWithout2010 <- fun_RelROCMom(MidAssets, period = "months",
                                 lookback = c(3, 6, 12), dateStart = "2010/",
                                 weights = c(1/6, 2/3, 1/6),
                                 NumAssets = 10, size = 1,
                                 UseTC = FALSE, TCmin = 0, TCrate = 0,
                                 savePlot = NULL,
                                 name = "Mid-Without-2010", width = 8, height = 6)
SmallWithout2017 <- fun_RelROCMom(SmallAssets, period = "months",
                                 lookback = c(3, 6, 12), dateStart = "2017/",
                                 weights = c(1/6, 2/3, 1/6),
                                 NumAssets = 10, size = 1,
                                 UseTC = FALSE, TCmin = 0, TCrate = 0,
                                 savePlot = NULL,
                                 name = "Small-Without-2017", width = 8, height = 6)
MidWithout2017 <- fun_RelROCMom(MidAssets, period = "months",
                                  lookback = c(3, 6, 12), dateStart = "2017/",
                                  weights = c(1/6, 2/3, 1/6),
                                  NumAssets = 10, size = 1,
                                  UseTC = FALSE, TCmin = 0, TCrate = 0,
                                  savePlot = NULL,
                                  name = "Mid-Without-2017", width = 8, height = 6)

# SmallWithout2010$plotCumReturn
# MidWithout2010$plotCumReturn
# SmallWithout2017$plotCumReturn
# MidWithout2017$plotCumReturn
#### Quarterly ####
SmallWithout2017Q <- fun_RelROCMom(SmallAssets, period = "quarters",
                                  lookback = c(3, 6, 12), dateStart = "2016/",
                                  weights = c(1/6, 2/3, 1/6),
                                  NumAssets = 10, size = 1,
                                  UseTC = FALSE, TCmin = 0, TCrate = 0,
                                  savePlot = NULL,
                                  name = "Small-Without-2018", width = 8, height = 6)
MidWithout2017Q <- fun_RelROCMom(MidAssets, period = "quarters",
                                lookback = c(3, 6, 12), dateStart = "2016/",
                                weights = c(1/6, 2/3, 1/6),
                                NumAssets = 10, size = 1,
                                UseTC = FALSE, TCmin = 0, TCrate = 0,
                                savePlot = NULL,
                                name = "Mid-Without-2018", width = 8, height = 6)
# SmallWithout2017Q$plotCumReturn
# MidWithout2017Q$plotCumReturn

#### Quarterly with TC####
Small2017Q <- fun_RelROCMom(SmallAssets, period = "quarters",
                                   lookback = c(3, 6, 12), dateStart = "2016/",
                                   weights = c(1/6, 2/3, 1/6),
                                   NumAssets = 10, size = 1,
                                   UseTC = TRUE, TCmin = 29, TCrate = 0.001,
                                   savePlot = NULL,
                                   name = "Small-Without-2018", width = 8, height = 6)
Mid2017Q <- fun_RelROCMom(MidAssets, period = "quarters",
                                 lookback = c(3, 6, 12), dateStart = "2016/",
                                 weights = c(1/6, 2/3, 1/6),
                                 NumAssets = 10, size = 1,
                                 UseTC = TRUE, TCmin = 29, TCrate = 0.001,
                                 savePlot = NULL,
                                 name = "Mid-Without-2018", width = 8, height = 6)
# Small2017Q$plotCumReturn
# Mid2017Q$plotCumReturn
#### Quarterly with TC and loading####
Small2017QLOAD <- fun_RelROCMom(SmallAssets, period = "quarters",
                            lookback = c(3, 6, 12), dateStart = "2016/",
                            weights = c(1/6, 2/3, 1/6),
                            NumAssets = 10, size = 15,
                            UseTC = TRUE, TCmin = 29, TCrate = 0.001,
                            savePlot = NULL,
                            name = "Small-Without-2018", width = 8, height = 6)
Mid2017QLOAD <- fun_RelROCMom(MidAssets, period = "quarters",
                          lookback = c(3, 6, 12), dateStart = "2016/",
                          weights = c(1/6, 2/3, 1/6),
                          NumAssets = 10, size = 15,
                          UseTC = TRUE, TCmin = 29, TCrate = 0.001,
                          savePlot = NULL,
                          name = "Mid-Without-2018", width = 8, height = 6)
# Small2017QLOAD$plotCumReturn
# Mid2017QLOAD$plotCumReturn
#### Err Quarterly with TC ####
SmallErr <- fun_RelErrMom(SmallAssets, period = "quarters",
                          lookback = c(3, 6, 12), dateStart = "2016/",
                          weights = c(1/6, 2/3, 1/6),
                          NumAssets = 10, size = 1,
                          UseTC = TRUE, TCmin = 30, TCrate = 0.001,
                          savePlot = NULL,
                          name = "Small-Error", width = 8, height = 6)
MidErr <- fun_RelErrMom(MidAssets, period = "quarters",
                        lookback = c(3, 6, 12), dateStart = "2016/",
                        weights = c(1/6, 2/3, 1/6),
                        NumAssets = 10, size = 1,
                        UseTC = TRUE, TCmin = 30, TCrate = 0.001,
                        savePlot = NULL,
                        name = "Mid-Error", width = 8, height = 6)
SmallErr$plotCumReturn
MidErr$plotCumReturn
SmallErr$StatTable
MidErr$StatTable

#### Err Quarterly with TC ####
SmallErrLoad <- fun_RelErrMom(SmallAssets, period = "quarters",
                          lookback = c(3, 6, 12), dateStart = "2016/",
                          weights = c(1/6, 2/3, 1/6),
                          NumAssets = 10, size = 15,
                          UseTC = TRUE, TCmin = 30, TCrate = 0.001,
                          savePlot = NULL,
                          name = "Small-Error-Load", width = 8, height = 6)
MidErrLoad <- fun_RelErrMom(MidAssets, period = "quarters",
                        lookback = c(3, 6, 12), dateStart = "2016/",
                        weights = c(1/6, 2/3, 1/6),
                        NumAssets = 10, size = 15,
                        UseTC = TRUE, TCmin = 30, TCrate = 0.001,
                        savePlot = NULL,
                        name = "Mid-Error-Load", width = 8, height = 6)
SmallErrLoad$plotCumReturn
MidErrLoad$plotCumReturn
