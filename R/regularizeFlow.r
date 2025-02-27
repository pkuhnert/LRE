#' @name regularization
#'
#' @title Internal functions to create a regularized flow
#' dataset using either a smoothing spline or rainfall record
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
#'
#' @importFrom "stats" "median"
#' @importFrom "stats" "sd"
#' @import "quantregForest"
#'
NULL

#' @rdname regularization
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


#' @rdname regularization
regularizeFlowR <- function(rainfall, dateTimeRainfall, flow, dateTimeFlow){


  if(length(rainfall) != length(dateTimeRainfall))
  {
    stop("The rainfall series and the rainfall datetime series must be the same length.")
  }
  if(class(rainfall) != "numeric")
  {
    stop("The rainfall series must be a numeric vector.")
  }
  if(class(dateTimeRainfall) != "Date" & class(dateTimeRainfall) != "POSIXlt" & class(dateTimeRainfall) != "POSIXct")
  {
    stop("The datetime series must be of class Date, POSIXlt or POSIXct.")
  }

  if(length(flow) != length(dateTimeFlow))
  {
    stop("The flow series and the flow datetime series must be the same length.")
  }
  if(class(flow) != "numeric")
  {
    stop("The rainfall series must be a numeric vector.")
  }
  if(any(class(dateTimeFlow) != "Date" & class(dateTimeFlow) != "POSIXlt" & class(dateTimeFlow) != "POSIXct" & class(dateTimeFlow) != "POSIXt"))
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








