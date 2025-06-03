#' @name diagnostic
#'
#' @title Produces diagnostic plots of a fitted loads model
#'
#' @description Produces a series of standard suite of diagnostics
#' for examining the fit of a loads model.
#'
#' @param object object of class "fitmodel" resulting from
#'   the \code{\link{FitModel}} function.
#'
#' @details Series of diagnostic plots for examining model fit.
#'
#' @return Produces the following diagnostics plots:
#' \code{residuals versus predicted} points should appear randomly
#'    distributed along the line at zero.
#' \code{normal Quantile-Quantile (Q-Q) plot} compares the quantiles
#'    from the theoretical distribution (i.e. normal) to that observed.
#'    If the 2 distributions are similar, then the points in the Q-Q plot
#'    will approximately lie on the line. Som deviation at the tails is
#'    acceptable.
#' \code{histogram of residuals} should appear roughly symmetric and bell
#'   shaped. No skewness (elongation of the tails) or kurtosis (thickening
#'   of the tails) should be present.
#' \code{predicted versus observed} points should appear close to the
#'   diagonal 0-1 line.
#' \code{predict versus obsered (WY)} plots of predicted versus observed
#'   for each water year.
#' \code{residuals versus predicted (WY)} plots of of residuals versus
#'   predicted for each water year.
#'
#'  @importFrom "stats" "resid"
#'  @importFrom "stats" "lag"
#'  @importFrom "mgcv" "predict.gam"
#'  @importFrom "nlme" "predict"
#'  @importFrom rlang .data
#'
#'  @references Kuhnert, P.M., Henderson, B.L., Lewis, S.E.,
#'  Bainbridge, Z.T., Wilkinson, S.N. and Brodie, J.E. (2012) Quantifying
#'  total suspended sediment export from the Burdekin River catchment using
#'  the loads regression estimator tool, Water Resources Research, 48,
#'  W04533,doi:10.1029/2011WR011080.
#'
#'  @seealso FitModel, qqnorm,, qqline
#'
#'  @keywords loads, diagnostics
#'
#'
#' @export
diagnostic.fitmodel <- function(object){

  if(class(object)[1] != "fitmodel")
    stop("Object is not of class 'fitmodel'.\n")



  if(length(object) == 2){
     nms <- names(object)
     y <- object$gam$y
     x <- object$gam
     if(any(nms == "gam") & any(nms == "gls")){
        pred <- predict(object$gls)
        resids <- resid(object$gls, type = "normalized")
     }
     else if(any(nms == "lme")){
        pred <- predict(object$lme, se.fit = TRUE)
        resids <- resid(object$lme, type = "normalized")
     }
     else if(any(nms == "gls")){
         pred <- predict(object$gam)
         resids <- resid(object$gam, type = "normalized")
     }

  }
  else{

     y <- object$y
     x <- object
     pred <- predict(object, se.fit = TRUE)$fit
     resids <- resid(object)
  }



  # Diagnostic plots
  #----------------- Standard Residual plots for Normal data --------------------#

  df <- data.frame(pred = pred, resids = resids, y = y)
  # Plot 1: Prediction versus residuals
  p1 <- ggplot(aes(pred, resids), data = df) + geom_point() + geom_hline(yintercept = 0) +
    xlab("Predicted") + ylab("Residuals") + ggtitle("Residuals vs Predicted")

  # Plot 2: Q-Q plot
  p2 <- gg_qq(resids) + xlab("Theoretical Quantiles") + ylab("Sample Quantiles") + ggtitle("Normal Q-Q Plot")

  # Plot 3 : histogram of residuals
  p3 <- ggplot(aes(resids), data = df, colour = "lightblue") + geom_histogram(bins = 30) + xlab("Residuals") +
    ggtitle("Histogram of Residuals")

  # Plot 4: observed vs predicted
  p4 <- ggplot(aes(y, pred), data = df) + geom_point() + xlab("Observed") + ylab("Predicted") +
    ggtitle("Predicted versus observed") + geom_abline(slope = 1, intercept = 0)

  pD <- marrangeGrob(list(p1, p2, p3, p4), ncol = 2, nrow = 2, top = "Diagnostic Plots")

  #-------------------------- Plot B: ACF plot ---------------------------------#
  acf <- acf(resids, plot = F)
  acf_df <- data.frame(
    acf = acf$acf,
    lag = acf$lag
  )
  ci <- 0.95
  clim <- qnorm((1 + ci)/2) / sqrt(acf$n.used)
  pacf <- ggplot(acf_df, aes(x = .data[["lag"]], y = .data[["acf"]])) +
    geom_segment(aes(xend = .data[["lag"]], yend = 0)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_hline(yintercept = c(-clim, clim), colour = "darkblue") +
    ggtitle("ACF of Residuals")

    list(pD = pD, pacf = pacf)


}

