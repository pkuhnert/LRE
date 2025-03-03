#' @name LRE-regularize-Internal
#'
#' @title features
#'
#' @description Creates rainfall feature variables for prediction of flow in a
#' random Forest
#'
#' @param series series
#' @param dateTime dateTime
#'
#' @import "lubridate"
#' @importFrom "stats" "median"
NULL

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
