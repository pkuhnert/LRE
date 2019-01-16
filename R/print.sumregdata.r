#' @name print.sumregdata
#'
#' @title Print out summaries from regularised data
#'
#' @description Print summarises an object of class "regdata"
#'
#' @param x regdata object
#' @param ... other parameters passed to \code{summary}
#'
#' @export


print.sumregdata <- function(x, ...){


  if(class(x)[1] != "sumregdata")
    stop("Object is not of class 'sumregdata'.\n")

  cat("Bias in flow and concentration sampling:\n")
  print(x$sumtab, ...)
  cat("\n\n")
  cat("Calculating quantiles from long term flow record with user defined cutoff.\n")
  cat(paste("No. of samples in the upper 2 percentile of flow: ", x$nc2pc, " (", round(x$nc2pc/nrow(x$CQ)*100),
            "% of samples collected) \n\n", sep = ""))
  cat("Distribution of flow sampling:\n")
  print(x$percsamp.tab, ...)


}
