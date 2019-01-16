#' @name LQflow
#'
#' @title Create linear and quadratic terms for flow
#'
#' @description Function used in 'FitModel' to create linear
#' and quadratic terms for flow.
#'
#' @param pQ flow
#' @param quad should quadratic terms be created? (Default: TRUE)
#'
#' @export


LQflow <- function(pQ, quad = TRUE){

       if(quad)
          term <- cbind(log(pQ), log(pQ)^2)
       else
          term <- cbind(log(pQ))

       term
}
