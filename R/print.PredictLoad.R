#' @name print.PredictLoad
#'
#' @title print.PredictLoad
#'
#' @description Outputs estimates to file
#'
#'
#' @param x object
#' @param ... other arguments passed to \code{print}
#'
#'
#' @references Kuhnert, P.M., Henderson, B.L., Lewis, S.E.,
#' Bainbridge, Z.T., Wilkinson, S.N. and Brodie, J.E. (2012)
#' Quantifying total suspended sediment export from the Burdekin
#' River catchment using the loads regression estimator tool,
#' Water Resources Research, 48, W04533,doi:10.1029/2011WR011080.
#'
#' @export
#'
print.PredictLoad <- function(x, ...){

  if(!inherits(x, "PredictLoad"))
    stop("Object is not of class 'PredictLoad'.\n")


     cat("Load Estimates: Annual \n")
     cat("  This is a data frame with dimensions", nrow(x$annual), " x ", ncol(x$annual), "\n\n")
     print(summary(x$annual))
     cat("\n")

     cat("Load Estimates: Daily \n")
     cat("  This is a data frame with dimensions", nrow(x$daily), " x ", ncol(x$daily), "\n\n")
     print(summary(x$daily), ...)
     cat("\n")

     cat("To write these to file, see ?write.csv or ?write.table")


}

