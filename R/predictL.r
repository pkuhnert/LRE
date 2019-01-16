#' @name predictL
#'
#' @title Predict Load
#'
#' @description Predicts the Load
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
#' @param ... other arguments to the predict function
#'
#'
#' @export
predictL <- function(object, objfix = NULL, x, flow.error = list(me = 0, ce = 0),
                     samp.unit = "hour", pvalue = 0.05)
  UseMethod("predictL")


