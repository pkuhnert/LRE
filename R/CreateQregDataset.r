#' CreateQregDataset
#' 
#' LRE Internal function.
#' 
#' @param Q Flow
#' @param samp.unit sampling unit
#' @param Qflush quantile corresponding to first flush
#' @param Ytype Year type: water year ("WY" or "WY2"), financial year ("FY")
#' @param Reg Regularisation parameters
#'
CreateQregDataset <- function(Q, samp.unit, Qflush, Ytype, Reg){

  startdate <- min(Q$Date)
  Q$days <- with(Q, as.numeric(difftime(Date, startdate, units = "days")))
  Qreg <- CreateRegDate(Q = Q, samp.unit = samp.unit, Ytype = Ytype)
#  Qreg$Date <- as.Date(Qreg$Date)
#  Q$Date <- as.Date(Q$Date)
  date.id <- match(Qreg$Date, Q$Date)


  if(any(is.na(date.id))){

    if(Reg$type == "qrforest"){

      Q.id <- match(Q$Date, Qreg$Date)
      Qreg$Flow <- rep(NA, nrow(Qreg))
      Qreg$Flow[Q.id] <- Q$Flow
      rainfall.df <- data.frame(rain = Reg$rainfall, Date = Reg$date)

      R.id <- match(rainfall.df$Date, Qreg$Date)
      rainfall.df$Discharge <- Qreg$Flow[R.id]
      testData <- rainfall.df
      names(testData) <- c("rain", "Date", "Discharge")
      regFlow <-  regularizeFlowR(testData[, "rain"], testData[, "Date"], testData$Discharge[!is.na(testData$Discharge)],
                             testData$Date[!is.na(testData$Discharge)])
      MissingDischarge <- is.na(testData$Discharge)
      TD.id <- (1:nrow(testData))[MissingDischarge]

      testData$Discharge[TD.id] <- regFlow[,1][TD.id]

      id <- match(Qreg$Date, testData$Date)
      Qreg$pQ <- testData$Discharge[id]

      nms <- names(Qreg)
      Qreg <- Qreg[, nms[nms != "Flow"]]

      df <- data.frame(Date = testData$Date, m = regFlow[,1], lci = regFlow[,4],
                       uci = regFlow[,12], Discharge = testData$Discharge,
                       Missing = MissingDischarge)
      pRF <- ggplot(aes(Date, Discharge, colour = Missing), data = df) + geom_line(size = 0.5)


      print(pRF)


    }
    else if(Reg$type == "ss"){

      Qreg <- regularizeFlowSS(Q = Q, Qreg = Qreg)

    }
    else{
      cat("No regularization considered. \n")

    }





  }
  else{

    Qreg$pQ <- Q$Flow[date.id]

  }

  ###################################
  # Creating terms for regularisation dataset
  ###################################

  # Trend Term
  start.date <- min(Qreg$Date)
  trend <- difftime(Qreg$Date, start.date, units = "days")
  Qreg$trend <- trend
  Qreg$trendY <- as.factor(Qreg$Y)
  levels(Qreg$trendY) <- paste(1:length(levels(Qreg$trendY)))
  Qreg$trendY <- as.numeric(as.vector(Qreg$trendY))
  # Month term 
  # If FY, month terms needs to run from July - June
  # If WY, month needs to run from October - September)
  if(Ytype == "WY"){
    Qreg$month <- as.POSIXlt(Qreg$Date)$mon + 1
    Qreg$month <- Qreg$month-9
    Qreg$month[Qreg$month == -8] <- 4
    Qreg$month[Qreg$month == -7] <- 5
    Qreg$month[Qreg$month == -6] <- 6
    Qreg$month[Qreg$month == -5] <- 7
    Qreg$month[Qreg$month == -4] <- 8
    Qreg$month[Qreg$month == -3] <- 9
    Qreg$month[Qreg$month == -2] <- 10
    Qreg$month[Qreg$month == -1] <- 11
    Qreg$month[Qreg$month == 0] <- 12
  }
  else{
    Qreg$month <- as.POSIXlt(Qreg$Date)$mon + 1
    Qreg$month <- Qreg$month-6
    Qreg$month[Qreg$month == -5] <- 7
    Qreg$month[Qreg$month == -4] <- 8
    Qreg$month[Qreg$month == -3] <- 9
    Qreg$month[Qreg$month == -2] <- 10
    Qreg$month[Qreg$month == -1] <- 11
    Qreg$month[Qreg$month == 0] <- 12
  }

  # Compute additional covariates
  # cumulative forms of flow
  #   Qreg

  Q.cov <- cumQ(Qreg)
  Qreg <- cbind(Qreg, Q.cov)
  Qreg$csdQ[is.nan(Qreg$csdQ)] <- 0
  # Day of Year (Load Day)
  yr <- as.POSIXlt(Qreg$Date)$year + 1900
  dateonly <- as.Date(Qreg$Date)
  Qreg$DayofYr <- abs(as.numeric(difftime(dateonly, as.Date(paste(yr,1,1,sep="-")),
                                          units = "days")))
  # Identify Window corresponding to a randomly selected FY
  y <- unique(Qreg$Y)
  y <- y[sample(1:length(y), size = 1)]
  ind <- (1:nrow(Qreg))[Qreg$Y == y]

  lvars <- with(Qreg, CreateLoadVars(pQ, csQ, Qflush = Qflush,
                                     samp.unit = samp.unit, Date, Y))
  Qreg$flush <- lvars$Flush
  Qreg$limb <- as.factor(lvars$limb)
  Qreg <- cbind(Qreg, as.matrix(lvars$dcum))
  # time index
  Qreg$time.index <- 1:nrow(Qreg)

 Qreg



}

