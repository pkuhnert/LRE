#' @title predictL.fitmodel
#'
#' @description Predicts the load and provides
#' uncertainty estimates for each water year.
#'
#'
#' @param object GAM modelling object resulting from
#' \code{\link{FitModel}} function.
#' @param objfix GAM modelling object resulting from \code{\link{FitModel}}
#' function when a correlation structure has been implemented.
#' @param x load data resulting from the \code{\link{CreateData}}
#' function.
#' @param flow.error a list containing the flow measurement and
#' cross-sectional error in terms of the coefficient of variation.
#' Default: none.
#' @param samp.unit Sampling unit for the loads calculation, "day" or "hour"
#' @param pvalue for confidence intervals. Default: 0.05 (95 %
#' confidence intervals)
#'
#' @details
#' Computes the loads estimates for each water year with standard
#' error estimates according to the p-value, sampling unit and flow
#' error specified. By default, the error due to flow rate is assumed
#' to be zero. Error can be included as a coefficient of variation
#' according to whether the error is due to the measurement of flow
#' (me) or the position of the gauge in the river, which is referred
#' to as "cross-sectional" (ce) error.
#'
#' @return
#' a \code{list} of class 'PredictLoad' with elements
#' \code{annual} annual loads estimates
#' \code{daily} daily loads estimates
#'  Standard \code{print} and \code{plot} functions apply.
#'
#' @references Kuhnert, P.M., Henderson, B.L., Lewis, S.E.,
#' Bainbridge, Z.T., Wilkinson, S.N. and Brodie, J.E. (2012)
#' Quantifying total suspended sediment export from the Burdekin
#' River catchment using the loads regression estimator tool,
#' Water Resources Research, 48, W04533,doi:10.1029/2011WR011080.
#'
#' @note Only water year calculations have been implemented.
#'
#' @seealso ReadInData, CreateData, FitModel
#'
#' @keywords uncertainty
#'
#' @importFrom "utils" "write.table"
#'
#' @export
#' 
predictL.fitmodel <- function(object, objfix = NULL, x, flow.error = list(me = 0, ce = 0),
                              samp.unit = "hour", pvalue = 0.05){

  if(class(object)[1] != "fitmodel")
    stop("Object is not of class 'fitmodel'.")


  pred <- EstChat(Qdat = x$Qreg, modobj = object, modobjfix = objfix)

  est <- EstLoadYS(pred, Qdat = x$Qreg, concY = x$CQ$Y,
                           alpha1 = flow.error$me, alpha2 = flow.error$ce,
                           modobj = object, samp.unit = samp.unit, se = TRUE)

  est <- Intervals(est, pvalue)
  loadest <- list(annual = est$loadest.annual, daily = est$loadest.daily)


  rdat <- x$CQ[,c(1,2,4,5)]
  names(rdat) <- c("Date", "C", "Qhat", "Year")

  class(loadest) <- "PredictLoad"

  list(loadest = loadest, regdata = pred$CQhat, rawCQ = rdat)
}

