#' @title summary.data
#'
#' @description Summary of data object
#'
#' @param object data object
#' @param ... other parameters passed to \code{summary}
#'
#' @importFrom stats median
#'
#'
#' @export

summary.data <- function(object, ...){

  if(!inherits(object, "data"))
    stop("Object is not of class 'data'.\n")

  if(!inherits(object, "data"))
    stop("Object is not of class 'data'.\n")


  # Flow data
  cat("Flow Data:\n")
  cat("---------\n")
  cat("Flow Collection Period: From ", as.character(object$Q[1,1])," to ",
                                       as.character(object$Q[nrow(object$Q),1]),"\n")

  sf <- mean(diff(object$Q[,1]))
  cat("No. of records: ", nrow(object$Q), "  Average sampling frequency: ", sf, attr(sf, "units"), "\n")
  cat(paste("Flow: \t min=", round(min(object$Q[,2]), 2),  " max=", round(max(object$Q[,2]), 2),

            "\n\t percentiles: 25th (", round(quantile(object$Q[,2], 0.2)), ") 50th (", round(quantile(object$Q[,2], 0.5)),
            ") 75th (", round(quantile(object$Q[,2], 0.75),2), ") 90th (", round(quantile(object$Q[,2], 0.9),2),
            ") 95th (", round(quantile(object$Q[,2], 0.95),2), ") 97.5th (", round(quantile(object$Q[,2], 0.975),2),
            ") 99th (", round(quantile(object$Q[,2], 0.99),2), ")",
             "\n\t median=", round(median(object$Q[,2]), 2), "\n\t mean=", round(mean(object$Q[,2]),2), "\n\n",
            sep = ""))


  # Concentration Data
  cat("Concentration Data:\n")
  cat("-------------------\n")
  cat(paste("Pollutant:  ", names(object$Conc)[2], "\n", sep = ""))
  cat("Concentration Collection Period: From ", as.character(object$Conc[1,1])," to ",
                                       as.character(object$Conc[nrow(object$Conc),1]),"\n")
  sf <- mean(diff(object$Conc$Date))
  cat("No. of records: ", nrow(object$Conc), "  Average sampling frequency: ", sf, attr(sf, "units"), "\n")
  cat(paste("Concentration: \t min=", round(min(object$Conc[,2]), 2),  " max=", round(max(object$Conc[,2]), 2),
            "\n\t percentiles: 25th (", round(quantile(object$Conc[,2], 0.2)), ") 50th (", round(quantile(object$Conc[,2], 0.5)),
            ") 75th (", round(quantile(object$Conc[,2], 0.75),2), ") 90th (", round(quantile(object$Q[,2], 0.9),2),
            ") 95th (", round(quantile(object$Conc[,2], 0.95),2), ") 97.5th (", round(quantile(object$Conc[,2], 0.975),2),
            ") 99th (", round(quantile(object$Conc[,2], 0.99),2), ")",
             "\n\t median=", round(median(object$Conc[,2]), 2), "\n\t mean=", round(mean(object$Conc[,2]),2), "\n\n",
            sep = ""))





}

