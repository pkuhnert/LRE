#' @name anova.fitmodel
#'
#' @title Analysis of Variance of fitted model
#'
#' @description Produces an analysis of variance of the "fitmodel" object
#'
#' @param object fitmodel object
#' @param ... other parameters passed to \code{anova}
#'
#' @importFrom "stats" "anova"
#'
#' @export

anova.fitmodel <- function(object, ...){

  if(class(object)[1] != "fitmodel")
    stop("Object is not of class 'fitmodel'.\n")




  if(length(object) == 2)
     anova(object$gam, ...)
  else
     NextMethod("anova")

}
