#' CreateCQDAtaset
#' 
#' LRE Internal function 
#' 
#' @param Conc Concentration data
#' @param Qreg Regularised flow data
#' @param startdate Starting date
#'
CreateCQDataset <- function(Conc, Qreg, startdate){

  Cm <- Conc
 # Cm$Date <- as.Date(Cm$Date)
  m <- AssignVal(Cm$Date, Qreg$Date)
  Cm$days <- as.numeric(difftime(Cm$Date, startdate, units = "days"))
  Cm$pQ <- Qreg$pQ[m]
 
  # Extract flow variables for Concentration dataset
  # Where concentration dates don't match, we linear interpolate between Qreg dates
  av <- AssignVal(Cm$Date, Qreg$Date)
  id <- av[is.na(m)]
  Cm$Y <- Qreg$Y[av]
  # DayofYr
  Cm$DayofYr <- Qreg$DayofYr[m]
  Cm$DayofYr[is.na(m)] <- apply(cbind(Qreg$DayofYr[id], Qreg$DayofYr[id+1]), 1, mean)
  # Month
  Cm$month <- Qreg$month[m]
  Cm$month[is.na(m)] <- apply(cbind(Qreg$month[id], Qreg$month[id+1]), 1, mean)
  # trend
  Cm$trend <- Qreg$trend[m]
  Cm$trend[is.na(m)] <- apply(cbind(Qreg$trend[id], Qreg$trend[id+1]), 1, mean)
  # trendY
  Cm$trendY <- Qreg$trendY[m]
  Cm$trendY[is.na(m)] <- apply(cbind(Qreg$trendY[id], Qreg$trendY[id+1]), 1, mean)
  # flows (csQ, cmQ, csdQ)
  Cm$csQ <- Qreg$csQ[m]
  Cm$csQ[is.na(m)] <- apply(cbind(Qreg$csQ[id], Qreg$csQ[id+1]), 1, mean)

  Cm$cmQ <- Qreg$cmQ[m]
  Cm$cmQ[is.na(m)] <- apply(cbind(Qreg$cmQ[id], Qreg$cmQ[id+1]), 1, mean)

  Cm$csdQ <- Qreg$csdQ[m]
  Cm$csdQ[is.na(m)] <- apply(cbind(Qreg$csdQ[id], Qreg$csdQ[id+1]), 1, mean)

  # Flush
  Cm$flush <- Qreg$flush[m]
  Cm$flush[is.na(m)] <- apply(cbind(Qreg$flush[id], Qreg$flush[id+1]), 1, mean)

  # MA variables
  Cm$MA1day <- Qreg$MA1day[m]
  Cm$MA1day[is.na(m)] <- apply(cbind(Qreg$MA1day[id], Qreg$MA1day[id+1]), 1, mean)

  Cm$MA2days <- Qreg$MA2days[m]
  Cm$MA2days[is.na(m)] <- apply(cbind(Qreg$MA2days[id], Qreg$MA2days[id+1]), 1, mean)

  Cm$MAweek <- Qreg$MAweek[m]
  Cm$MAweek[is.na(m)] <- apply(cbind(Qreg$MAweek[id], Qreg$MAweek[id+1]), 1, mean)

  Cm$MAmonth <- Qreg$MAmonth[m]
  Cm$MAmonth[is.na(m)] <- apply(cbind(Qreg$MAmonth[id], Qreg$MAmonth[id+1]), 1, mean)

  Cm$MA6months <- Qreg$MA6months[m]
  Cm$MA6months[is.na(m)] <- apply(cbind(Qreg$MA6months[id], Qreg$MA6months[id+1]), 1, mean)

  Cm$MA12months <- Qreg$MA12months[m]
  Cm$MA12months[is.na(m)] <- apply(cbind(Qreg$MA12months[id], Qreg$MA12months[id+1]), 1, mean)

  # limb
  Cm$limb <- Qreg$limb[m]
  Cm$limb[is.na(m)] <- apply(cbind(as.numeric(as.vector(Qreg$limb[id])),
                                   as.numeric(as.vector(Qreg$limb[id+1]))), 1, function(x)
                                     as.numeric(names(table(x))[table(x) == max(table(x))])[1])
  Cm$limb <- factor(as.vector(Cm$limb) , levels = c("0", "-1", "1"))
  #time.index
  Cm$time.index <- Qreg$time.index[m]

  # Average observations with the same time index
  CQ <- data.frame(Date = with(Cm, tapply(Date, time.index, mean)),
                   Conc = with(Cm, tapply(Conc, time.index, mean)),
                   days = with(Cm, tapply(days, time.index, mean)),
                   pQ = with(Cm, tapply(pQ, time.index, mean)),
                   Y = with(Cm, tapply(pQ, time.index, unique)),
                   DayofYr = with(Cm, tapply(DayofYr, time.index, mean)),
                   month = with(Cm, tapply(month, time.index, mean)),
                   trend = with(Cm, tapply(trend, time.index, mean)),
                   trendY = with(Cm, tapply(trendY, time.index, mean)),
                   flush = with(Cm, tapply(flush, time.index, mean)),
                   csQ = with(Cm, tapply(csQ, time.index, mean)),
                   cmQ = with(Cm, tapply(cmQ, time.index, mean)),
                   csdQ = with(Cm, tapply(csdQ, time.index, mean)),
                   MA1day = with(Cm, tapply(MA1day, time.index, mean)),
                   MA2days = with(Cm, tapply(MA2days, time.index, mean)),
                   MAweek = with(Cm, tapply(MAweek, time.index, mean)),
                   MAmonth = with(Cm, tapply(MAmonth, time.index, mean)),
                   MA6months = with(Cm, tapply(MA6months, time.index, mean)),
                   MA12months = with(Cm, tapply(MA12months, time.index, mean)),
                   limb = with(Cm, tapply(limb, time.index, unique)),
                   time.index = with(Cm, tapply(time.index, time.index, mean)))
  index <- tapply(1:nrow(Cm), Cm$time.index, function(x) x[1])
  CQ$Date <- Cm$Date[index]
  CQ$Y <- Cm$Y[index]
  CQ$limb <- Cm$limb[index]

  list(CQ = CQ, Cm = Cm)

}

