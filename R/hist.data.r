#' @name hist.data
#'
#' @title histogram
#'
#' @description Produce a histogram of the raw data
#'
#' @param object data object
#'
#' @import ggplot2
#' @import gridExtra
#' @importFrom rlang .data
#'
#' @export
#'
hist.data <- function(object){

  if(!inherits(object, "data"))
    stop("Object is not of class 'data'.\n")


  # Histograms

  object$Q$lFlow <- log(object$Q[,2])
  p1 <- ggplot(data = object$Q, aes(x = .data[[names(object$Q)[2]]])) +
    geom_histogram(fill = "purple", bins = 20) + xlab("Flow (m3/s)") +
    ggtitle("Q")
  p2 <- ggplot(data = object$Q, aes(x = .data[["lFlow"]])) + 
    geom_histogram(fill = "orange", bins = 20) +
    xlab("log(Flow)") + ggtitle("log(Q)")
  df <- data.frame(Conc = object$Conc[,2], logConc = log(object$Conc[,2]))
  p3 <- ggplot(data = df, aes(x = .data[["Conc"]])) + 
    geom_histogram(fill = "purple", bins = 20) +
    xlab(paste(names(object$Conc)[2], "mg/L", sep = "")) + ggtitle(names(object$Conc)[2])
  p4 <- ggplot(data = df, aes(x = .data[["logConc"]])) + geom_histogram(fill = "orange", bins = 20) +
    xlab(paste("log(", names(object$Conc)[2], ")", sep = "")) +
    ggtitle(paste("log(", names(object$Conc)[2], ")", sep = ""))

  p <- marrangeGrob(list(p1, p2, p3, p4), ncol = 2, nrow = 2,
                    top = "Exploratory Plots of Flow and Concentration")


  p
}
