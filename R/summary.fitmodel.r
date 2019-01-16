#' @name summary.fitmodel
#'
#' @title Summarises fitted model.
#'
#' @description summarises an object of class "fitmodel"
#'
#' @param object fitmodel object
#' @param ... other arguments to \code{summary}
#'
#' @importFrom "nlme" "intervals"
#'
#' @export

summary.fitmodel <- function(object, ...){

  if(class(object)[1] != "fitmodel")
    stop("Object is not of class 'fitmodel'.\n")


   ARfit <- object$lme
  if(!is.null(ARfit)){
     model.sum <- summary(object$gam, ...)
     corr.sum <- summary(ARfit$modelStruct$corStruct, ...)
     corr.int <-  try(intervals(ARfit, which = "var-cov")$corStruct, silent = TRUE)
     if(class(corr.int) == "try-error")
        corr.int <- NULL
     list(Model = model.sum, Corr = corr.sum, CorrInt = corr.int)
  }
  else
     NextMethod("summary")


}
