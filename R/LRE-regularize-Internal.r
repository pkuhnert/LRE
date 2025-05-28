#' @name LRE-regularize-Internal
#' 
#' @title Internal functions to create a regularized flow
#' dataset using either a smoothing spline or rainfall record using
#' Random Forests.
#'
#' @description Creates a regularised flow dataset
#' using a smoothing spline or rainfall record using
#' Random Forests. Predictions produced from the smoothing
#' spline approach are checked to ensure predictions do not
#' go outside the range of the raw data. If this occurs for any year
#' a warning is issued for the user to check.
#'
#'
#' @param Q Q
#' @param Qreg Qreg
#' @param rainfall rainfall
#' @param dateTimeRainfall dateTimeRainfall
#' @param flow flow
#' @param dateTimeFlow dateTimeFlow
#' @param method spline method for interpolation
#' @param series series
#' @param dateTime dateTime
#'
#' @importFrom stats splinefun
#' @importFrom stats median
#' @importFrom stats sd
#' @importFrom quantregForest quantregForest
#' @importFrom stats predict
#' @import lubridate
#' @importFrom rlang .data
#' 
#' @aliases features regularizeFlowSS regularizeFlowR FitSmQ
#' @keywords internal
NULL

#' @keywords internal
features <- function(series, dateTime){


  if(length(series) != length(dateTime))
  {
    stop("The rainfall series and the rainfall datetime series must be the same length.")
  }
  if(!inherits(series, "numeric"))
  {
    stop("The rainfall series must be a numeric vector.")
  }
  if(!inherits(dateTime, "Date")  & !inherits(dateTime, "POSIXlt") & !inherits(dateTime, "POSIXct"))
  {
    stop("The datetime series must be of class Date, POSIXlt or POSIXct.")
  }

  windows <- c(1:10, 20, 30)
  windowFeaturesMin <- matrix(NA, length(series), length(windows))
  windowFeaturesMax <- matrix(NA, length(series), length(windows))
  windowFeaturesMean <- matrix(NA, length(series), length(windows))
  windowFeaturesMeanDiff <- matrix(NA, length(series), length(windows))
  windowFeaturesMedian <- matrix(NA, length(series), length(windows))
  differences <- c(0, diff(series))
  timeDifferences <- c(0, diff(dateTime))
  sinDOY <- sin(2*pi*yday(dateTime)/365.25)
  cosDOY <- cos(2*pi*yday(dateTime)/365.25)
  for(w in 1:length(windows)){
    for(i in 1:length(series)){
      timeLag <- dateTime[i] - dateTime
      ind <- which(timeLag < windows[w] & timeLag >= 0)
      windowFeaturesMin[i, w] <- min(series[ind])
      windowFeaturesMax[i, w] <- max(series[ind])
      windowFeaturesMean[i, w] <- mean(series[ind])
      windowFeaturesMeanDiff[i, w] <- mean(series[i] - series[ind])
      windowFeaturesMedian[i, w] <- median(series[ind])
    }
  }
  allFeatures <- cbind(series, differences, timeDifferences, sinDOY, cosDOY, windowFeaturesMin, windowFeaturesMax, windowFeaturesMean, windowFeaturesMeanDiff, windowFeaturesMedian)
  colnames(allFeatures) <- c("series", "difference", "timeDifference", "sinDOY", "cosDOY", paste0("windowFeaturesMin", windows), paste0("windowFeaturesMax", windows), paste0("windowFeaturesMean", windows), paste0("windowFeaturesMeanDiff", windows), paste0("windowFeaturesMedian", windows))

  allFeatures
}


#' @keywords internal
regularizeFlowSS <- function(Q, Qreg){
  
  val <- FitSmQ(Q = Q, Qreg = Qreg)
  
  Qreg <- val$Qreg
  smf <- val$smf
  medvals <- with(Q, tapply(Flow, Y, median))
  
  y.id <- match(Qreg$Y, names(medvals))
  
  maxvalsQ <- with(Q, tapply(Flow, Y, max))
  maxvalsQreg <- with(Qreg, tapply(pQ, Y, max))
  if(any(maxvalsQreg > maxvalsQ))
    warning("Regularisation has identified some predicted flows outside what was measured.\n")
  
  
  Qreg
  
}


#' @keywords internal
regularizeFlowR <- function(rainfall, dateTimeRainfall, flow, dateTimeFlow){
  
  
  if(length(rainfall) != length(dateTimeRainfall))
  {
    stop("The rainfall series and the rainfall datetime series must be the same length.")
  }
  if(!inherits(rainfall, "numeric"))
  {
    stop("The rainfall series must be a numeric vector.")
  }
  if(!inherits(dateTimeRainfall, "Date") & !inherits(dateTimeRainfall, "POSIXlt") & 
     !inherits(dateTimeRainfall, "POSIXct"))
  {
    stop("The datetime series must be of class Date, POSIXlt or POSIXct.")
  }
  
  if(length(flow) != length(dateTimeFlow))
  {
    stop("The flow series and the flow datetime series must be the same length.")
  }
  if(!inherits(flow, "numeric"))
  {
    stop("The rainfall series must be a numeric vector.")
  }
  if(any(!inherits(dateTimeFlow, "Date") & !inherits(dateTimeFlow, "POSIXlt") & 
         !inherits(dateTimeFlow, "POSIXct") & !inherits(dateTimeFlow, "POSIXt")))
  {
    stop("The datetime series must be of class Date, POSIXlt or POSIXct.")
  }
  
  rainfallFeatures <- features(rainfall, dateTimeRainfall)
  colnames(rainfallFeatures) <- paste0("rain.", colnames(rainfallFeatures))
  
  pairedInd <- rep(NA, length(dateTimeFlow))
  for(i in 1:length(dateTimeFlow))
  {
    relevantIndex <- AssignVal(dateTimeFlow[i], dateTimeRainfall)
    pairedInd[i] <- relevantIndex
  }
  
  df.train <- data.frame(cbind(flow, rainfallFeatures[pairedInd, ]))
  df.pred <- data.frame(rainfallFeatures)
  qrf <- quantregForest(x=df.train[, -1], y=df.train[, 1])
  conditionalQuantiles.mean <- predict(qrf, df.pred, what = mean)
  conditionalQuantiles.sd <- predict(qrf, df.pred, what = sd)
  q <- c(0.005, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.995)
  conditionalQuantiles <- predict(qrf, df.pred, what = q)
  output <- cbind(conditionalQuantiles.mean, conditionalQuantiles.sd, conditionalQuantiles)
  colnames(output) = c("mean", "sd", paste0("quantile_", q))
  
  output
}


#' @keywords internal
FitSmQ <- function(Q, Qreg, method = "monoH.FC"){
  
  
  # Using splinefun
  f <- splinefun(Q$days, log(Q$Flow), method = method)
  Qreg$pQ <- exp(f(Qreg$days))
  
  Q$lFlow <- log(Q$Flow)
  Qreg$lpQ <- log(Qreg$pQ)
  pA1 <- ggplot(aes(x = .data[["days"]], y = .data[["lFlow"]]), data = Q) + geom_line()
  pA2 <- pA1 + xlab("Days") + ylab("log(Flow)") + ggtitle("log-scale")
  pA3 <- pA2 + geom_point(aes(x = .data[["days"]], y = .data[["lpQ"]]), 
                          data = Qreg, colour = "purple")
  
  pB1 <- ggplot(aes(x = .data[["days"]], y = .data[["Flow"]]), data = Q) + geom_line()
  pB2 <- pB1 + xlab("Days") + ylab("Flow (CM/s)") + ggtitle("natural scale")
  pB3 <- pB2 + geom_point(aes(x = .data[["days"]], y = .data[["pQ"]]), 
                          data = Qreg, colour = "purple")
  
  p <- marrangeGrob(list(pA3, pB3), ncol = 1, nrow = 2, top = "Regularisation")
  print(p)
  
  list(smf = f, Qreg = Qreg)
  
}








