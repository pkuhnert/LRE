#' CreateData
#'
#' @title Creates datasets that will be used in loads modelling
#'   and prediction
#'
#' @description Creates a regularised flow dataset for prediction and a
#' modelling dataset with flow metrics that can be used in regression
#' modelling.
#'
#'
#' @param Q \code{n x 2} matrix of flow (Date, Flow) where flow is measured in cumecs
#' @param Qflush Qflush
#' @param Conc $nxp$ matrix of concentrations  (Date, Conc1, Conc2, ...) where
#' concentration is measured in mg/L
#' @param date.range list containing date ranges for modelling and prediction
#' datasets (default: NULL)
#' @param samp.unit sampling unit 'hour' or 'daily'
#' @param Ytype water year "WY" or "WY2" or financial year "FY" (note only WY
#'   implemented at this stage)
#' @param Reg Regularisation check
#'
#' @details This function creates two datasets that are used in
#' modelling and prediction. A third dataset is just a copy of
#' the observed flow data.
#'
#' Inputs into the function are flow and concentration datasets that
#' have been read in using the \code{ReadInData} function or
#' are of class "data".
#' @return
#'  a \code{list} of class 'regdata' containing:
#' \code{CQ} flow and concentration modelling dataset
#' \code{Qreg} regularised dataset
#' \code{Q} Observed flow matrix
#' Standard \code{summary} and \code{plot} functions apply.
#'
#' @references Kuhnert, P.M., Henderson, B.L., Lewis, S.E.,
#' Bainbridge, Z.T., Wilkinson, S.N. and Brodie, J.E. (2012)
#' Quantifying total suspended sediment export from the Burdekin
#' River catchment using the loads regression estimator tool,
#' Water Resources Research, 48, W04533,doi:10.1029/2011WR011080.
#'
#' @seealso ReadInData
#'
#' @keywords loads, metrics
#'
#' @examples
#' #Reading in data already stored in R
#' library(LRE)
#' burdR <- ReadInDataFromR(x.C = burdRC, x.Q = burdRQ)
#'
#' # Create Dataset (no need for regularisation here as have full data)
#' date.rangeM <- c("1973-12-02", "2015-06-30")
#' date.rangeP <- c("1973-12-02", "2015-06-30")
#' loaddata <- CreateData(Q = burdR$Q, Conc = burdR$Conc,
#'                 date.range = list(model = date.rangeM, pred = date.rangeP, hour = FALSE),
#'                 samp.unit = "day", Ytype = "WY", Qflush = 0.9,
#'                 Reg = list(type = "none", rainfall = NULL, date = NULL))
#' p <- plot(loaddata, Type = "WY")
#' summary(loaddata)
#'
#' @export


CreateData <-
function(Q, Conc, date.range = list(model = NULL, pred = NULL, hour = FALSE), samp.unit = "hour", Ytype = "WY",
                    Qflush = 0.9, Reg = list(type = "ss", rainfall = NULL, date = NULL)){


  if(missing(Q))
    stop("Flow data missing. \n")
  if(missing(Conc))
    stop("Concentration data missing. \n")
  if(missing(date.range))
    stop("Date ranges for modelling, prediction are not provided. \n")
  if(missing(Reg))
    stop("Regularization parameters not provided. \n")


  if(!(samp.unit == "hour" | samp.unit == "day"))
    stop("Sampling unit needs to be either 'hour' or 'day'.\n")
  if(!(Ytype == "WY" | Ytype == "WY2" | Ytype == "FY"))
    stop("Ytype needs to be either specified as a water year (WY or WY2) or financial year (FY).\n")
  if(is.nan(Qflush) | is.na(Qflush) | Qflush < 0 | Qflush > 1)
    stop("Qflush needs to be a value between 0 and 1.\n")
  if(!(Reg$type == "ss" | Reg$type == "qrforest" | Reg$type == "none" ))
    stop("Regularization either has to be using a smoothing spline (ss), a Random Forest (qrforest) or 'none' for
         no regularization.\n")



   # Creating a combined dataset
   if(!is.null(date.range$model)){
      if(date.range$hour)
        date.range$model <- as.POSIXct(date.range$model, format = "%Y-%m-%d %H:%M" , tz = "GMT")
      else
        date.range$model <- as.POSIXct(date.range$model, format = "%Y-%m-%d" , tz = "GMT")
   }
   if(!is.null(date.range$pred)){
     if(date.range$hour)
      date.range$pred <- as.POSIXct(date.range$pred, format = "%Y-%m-%d %H:%M" , tz = "GMT")
     else
       date.range$pred <- as.POSIXct(date.range$pred, format = "%Y-%m-%d" , tz = "GMT")
   }


   val <- CreateLD(Q, Conc, date.range, samp.unit, Ytype,
              Qflush = Qflush, Reg = Reg)

   val <- list(CQ = val$Cm, Qreg = val$Qreg, Q = Q)
   class(val) <- "regdata"
   val
}

