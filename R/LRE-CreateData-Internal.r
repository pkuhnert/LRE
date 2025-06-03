#' @name LRE-CreateData-Internal
#'
#' @title Internal functions for \code{CreateData} function.
#'
#' @description Internal functions for curating the data
#'
#'
#' @param Q Flow data
#' @param Conc Concentration data
#' @param date.range Date range
#' @param samp.unit Sampling unit
#' @param Ytype Year type. One of water year ("WY" or "WY2") or financial year ("FY")
#' @param Qflush Quantile for "first flush"
#' @param Reg Parameters for regularisation (type, rainfall, date)
#' @param Qreg Regularised flow data
#' @param startdate Starting date
#' @param x vector of x-values
#' @param y vector of y-values
#' @param csQ Cumulative sum of flow
#' @param samp.unit sampling unit
#' @param Date Date vector
#' @param Y Year type
#' 
#' @aliases AssignVal CreateCQDataset CreateLD CreateLoadVars CreateQregDataset CreateRegDate PlotCQ
#' @keywords internal
#'
#'
#' @importFrom TTR EMA
#' @importFrom rlang .data
#'
NULL

#' @keywords internal
CreateLD <- function(Q, Conc, date.range, samp.unit, Ytype,
                     Qflush, Reg = list(type = "none", rainfall = NULL, date = NULL)){
  
  
  #---------------- Preliminaries --------------------------------------#
  # Remove any missing records in Q or Conc
  Q <- na.omit(Q)
  if(ncol(Conc) != 2) stop("Concentration dataset should have 2 columns: Date, Concentration\n")
  names(Conc) <- c("Date", "Conc")
  Conc <- na.omit(Conc)
  # replace 0 records of flow with 0.01, a small value
  # avoids log(0)
  Q$Flow[Q$Flow == 0] <- 0.01
  # Create a Y variable: for estimating loads each year (WY/FY)
  Q$Y <- CreateY(Q$Date, Ytype = Ytype)
  
  #-----------------------  Extract modelling dataset ----------------------#
  
  if(!is.null(date.range$model)){
    # Restrict concentration data to specific dates set by user
    dateR <- as.Date(as.POSIXct(date.range$model, format = "%d-%m-%Y %H:%M:%S", tz = "GMT"),
                     format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
    mind <- dateR[1]
    maxd <- dateR[2]
  }
  else{
    # Restrict flow data
    mind <- as.Date(min(Conc$Date))
    maxd <- as.Date(max(Conc$Date))
  }
  indC <- with(Conc, as.Date(Date) >= mind & as.Date(Date) <= maxd)
  Conc <- Conc[indC,]
  
  #----------------------------- Extract prediction dataset --------------------------------#
  if(!is.null(date.range$pred)){
    # Restrict flow data to specific dates set by user
    dateR <- as.Date(as.POSIXct(date.range$pred, format = "%d-%m-%Y", tz = "GMT"), format = "%d-%m-%Y", tz = "GMT")
    mind <- dateR[1]
    maxd <- dateR[2]
  }
  else{
    # Restrict flow to concentration data
    mind <- as.Date(min(Conc$Date))
    maxd <- as.Date(max(Conc$Date))
  }
  ind <- as.Date(Q$Date) >= mind & as.Date(Q$Date) <= maxd
  Q <- Q[ind,]
  
  #-------------------------------- REGULARISATION --------------------------------------------#
  
  Qreg <- CreateQregDataset(Q = Q, samp.unit = samp.unit, Qflush = Qflush, Ytype = Ytype,
                            Reg = Reg)
  
  #----------------------------- Creating modelling dataset by matching ------------------------#
  #----------------------------- concentration dates with flow dates ---------------------------#
  
  val <- CreateCQDataset(Conc = Conc, Qreg = Qreg, startdate = min(Q$Date))
  
  CQ <- val$CQ
  Cm <- val$Cm
  
  PlotCQ(Cm, Qreg)
  
  list(Cm = CQ, Qreg = Qreg)
}



#' @keywords internal
CreateQregDataset <- function(Q, samp.unit, Qflush, Ytype, Reg){
  
  startdate <- min(Q$Date)
  Q$days <- with(Q, as.numeric(difftime(Date, startdate, units = "days")))
  Qreg <- CreateRegDate(Q = Q, samp.unit = samp.unit, Ytype = Ytype)
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
      regFlow <-  regularizeFlowR(testData[, "rain"], testData[, "Date"], 
                                  testData$Discharge[!is.na(testData$Discharge)],
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
      pRF <- ggplot(aes(x = .data[["Date"]], y = .data[["Discharge"]], 
                        colour = .data[["Missing"]]), data = df) + geom_line(linewidth = 0.5)
      
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
  
  #------------------------------------------------------------------
  # Creating terms for regularisation dataset
  #------------------------------------------------------------------
  
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
  #   Qreg
  
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

#' @keywords internal
PlotCQ <- function(Conc, Q){
  
  Conc$lConc <- log(Conc$Conc)
  Conc$lpQ <- log(Conc$pQ)
  p1 <- ggplot(aes(x = .data[["Date"]], y = .data[["lConc"]]), data = Conc) + geom_line() + 
    ylab("log(Concentration)")
  p2 <- ggplot(aes(x = .data[["Date"]], y = .data[["lpQ"]]), data = Conc) + geom_line() + 
    ylab("log(Flow)")
  
  minD <- min(Conc$Date)
  maxD <- max(Conc$Date)
  id <- (Q$Date >= minD & Q$Date <= maxD)
  Qsub <- Q[id,]
  Qsub$lpQ <- log(Qsub$pQ)
  p3 <- ggplot(aes(x = .data[["Date"]], y = .data[["lpQ"]]), data = Qsub) + 
    geom_line(colour = "blue")
  
  p <- marrangeGrob(list(p1, p2, p3), ncol = 1, nrow = 3, top = "Relationship between concentration and flow")
  
  
  p
}


#' @keywords internal
CreateCQDataset <- function(Conc, Qreg, startdate){
  
  Cm <- Conc
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


#' @keywords internal
AssignVal <- function(x, y){
  
  id <- sapply(as.numeric(x), function(x, y){
    val <- abs(x-y)
    (1:length(val))[val == min(val)][1]}  , as.numeric(y))
  
  id
}

#' @keywords internal
CreateRegDate <- function(Q, samp.unit, Ytype){
  
  start.date <- min(Q$Date)
  end.date <- max(Q$Date)
  
  if(samp.unit == "hour")
    len <- 60*60
  else if(samp.unit == "day")
    len <- 60*60*24
  else stop("sampling unit not defined\n")
  
  d <- seq(start.date, end.date, by = len)
  
  Qreg <- data.frame(Date = d)
  days <- with(Qreg, as.numeric(difftime(Date, start.date, units = "days")))
  Y <- CreateY(d, Ytype = Ytype)
  
  data.frame(Date = d, days = days, Y = Y)
  
}


#' @keywords internal
CreateLoadVars <- function(Q, csQ, Qflush, samp.unit, Date, Y){
  
  # Identify flush
  qY <- data.frame(qpc = tapply(Q, Y, function(x) quantile(x, Qflush)))
  qY$Y <- row.names(qY)
  row.names(qY) <- NULL
  id <- match(Y, qY$Y)
  qpc <- qY$qpc[id]
  
  # Determine where there is a big flow
  Bflow <- ifelse(Q <= qpc, 0, log(Q))
  
  # Calculate the limb: 0=no flush; 1= flush in rising limb; -1 = flush in decreasing limb
  d <- c(0, diff(Q))
  limb <- (Q > qpc) * sign(d)
  df <- data.frame(Date = Date, Q = Q, Limb = limb)
  p1 <- ggplot(df, aes(x = .data[["Date"]], y = .data[["Q"]])) + geom_line() + ylab("Flow")
  p2 <- ggplot(df, aes(x = .data[["Date"]], y = .data[["Limb"]]), col = "blue") + geom_line()
  p_flush <- marrangeGrob(list(p1, p2), ncol = 1, nrow = 2, top = "Rising/Falling Limb")
  
  # Calculate MA terms
  if(samp.unit == "hour"){
    
    # past 24 hours, past 2 days, past week, past month, past 3 months, past 6 months, past 12 months
    discount <- c(24, 24*2, 24*7, 24*30, 24*30*3, 24*30*6, 24*30*12)
    ema.sm <- sapply(discount, function(x) EMA(Q, n = x))
    # now replace NAs with MA of values up to n, where n is the largest n used
    replNA <- unlist(lapply(1:(24*30*12), function(x){ tmp <- EMA(Q, n = x)
    tmp[!is.na(tmp)][1]}))
    for(i in 1:ncol(ema.sm)){
      ema.sm[,i][is.na(ema.sm[,i])] <- replNA[is.na(ema.sm[,i])]
    }
    
    
  }
  else if(samp.unit == "day"){
    # past day, past 2 days, past week, past month, past 3 months, past 6 months, past 12 months
    discount <- c(1, 2, 7, 30, 30*3, 30*6, 30*12)
    ema.sm <- sapply(discount, function(x) EMA(Q, n = x))
    # now replace NAs with MA of values up to n, where n is the largest n used
    replNA <- unlist(lapply(1:(30*12), function(x){ tmp <- EMA(Q, n = x)
    tmp[!is.na(tmp)][1]}))
    for(i in 1:ncol(ema.sm)){
      ema.sm[,i][is.na(ema.sm[,i])] <- replNA[is.na(ema.sm[,i])]
    }
    
    
  }
  else stop("The sampling unit specified has not been implement. Choose either 'day' or 'hour'.")
  
  dframe <- data.frame(ema.sm)
  nms <- paste("MA", c("1day", "2days", "week", "month", "3months", "6months", "12months"), sep = "")
  names(dframe) <- nms
  
  dat <- data.frame(Q = Q, ema.sm, Date = Date)
  names(dat) <- c("Q", nms, "Date")
  p1 <- ggplot(data = dat, aes(Date, Q)) + geom_line(col = "black")
  p2 <- p1 + geom_line(data = dat, aes(x = .data[["Date"]], y = .data[["MA1day"]]), col = "red")
  p3 <- p2 + geom_line(data = dat, aes(x = .data[["Date"]], y = .data[["MA2days"]]), col = "orange")
  p4 <- p3 + geom_line(data = dat, aes(x = .data[["Date"]], y = .data[["MAweek"]]), col = "yellow")
  p5 <- p4 + geom_line(data = dat, aes(x = .data[["Date"]], y = .data[["MAmonth"]]), col = "green")
  p6 <- p5 + geom_line(data = dat, aes(x = .data[["Date"]], y = .data[["MA3months"]]), col = "lightblue")
  p7 <- p6 + geom_line(data = dat, aes(x = .data[["Date"]], y = .data[["MA6months"]]), col = "blue")
  p_lagterms <- p7 + geom_line(data = dat, aes(x = .data[["Date"]], y = .data[["MA12months"]]), col = "purple")
  
  
  list(Flush = qpc, limb = limb, dcum = dframe, p_lagterms = p_lagterms, p_flush = p_flush)
  
}


