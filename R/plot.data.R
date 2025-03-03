#' @name plot.data
#'
#' @title Plot data objects
#'
#' @description Plot object of class 'data'
#'
#' @param x data object
#' @param logt log transformation. Default: FALSE
#' @param ... other parameters passed to \code{plot}
#'
#' @import "ggplot2"
#' @import "gridExtra"
#'
#' @export

plot.data <- function(x, logt = FALSE, ...){


    if(!inherits(x, "data"))
      stop("Object is not of class 'data'.")

    # work out xlimits so dates match up
    xlimit.Q <- range(x$Q[,1], na.rm = TRUE)
    xlimit.C <- range(x$Conc[,1], na.rm = TRUE)

    # date period

    dd.Q <- as.POSIXlt(xlimit.Q)
    dd.Q <- with(dd.Q, paste("(", mon[1]+1, "/", year[1]+1900, "-",
                             mon[2]+1, "/", year[2]+1900, ")",sep = ""))
    dd.C <- as.POSIXlt(xlimit.C)
    dd.C <- with(dd.C, paste("(", mon[1]+1, "/", year[1]+1900, "-",
                             mon[2]+1, "/", year[2]+1900, ")",sep = ""))

    if(logt){
       flow <- x$Q[,2]
       flow[flow == 0] <- 0.01
       flow <- log(flow)
       ylab.flow <- "log(Flow)"
       main.flow <- "Flow Data (log-scale)"
       conc <- x$Conc[,2]
       conc[conc == 0] <- 0.5*min(conc[conc != 0])
       conc <- log(conc)
       ylab.conc <- paste("log(", names(x$Conc)[2], ")", sep = "")
       main.conc <- "Concentration Data (log-scale)"
       }
    else{
       flow <- x$Q$Flow
       ylab.flow <- "Flow (m3/s)"
       main.flow <- "Flow Data"
       conc <- x$Conc[,2]
       ylab.conc <- paste(names(x$Conc)[2], "(mg/L)")
       main.conc <- "Concentration Data"
    }

    newdatQ <- data.frame(Date = x$Q[,1], Flow = flow)
    newdatC <- data.frame(Date = x$Conc[,1], Conc = conc)

    names(newdatQ) <- names(x$Q)
    names(newdatC) <- names(x$Conc)


    p1 <- ggplot(data = newdatQ, aes_string(names(newdatQ)[1], names(newdatQ)[2])) + geom_line() + xlab(paste("Flow Collection Period", dd.Q)) +
      ylab(ylab.flow) + ggtitle(main.flow) + geom_rug(data = newdatC,
                                                      aes_string(names(newdatC)[1], names(newdatC)[2]), sides = "b")
    p2 <- ggplot(data = newdatC, aes_string(names(newdatC)[1], names(newdatC)[2])) +
      geom_point(col = "blue") + xlab(paste("Concentration Collection Period", dd.C)) +
      ylab(ylab.conc) + xlim(xlimit.Q) + ggtitle(main.conc)

    p <- marrangeGrob(list(p1, p2), ncol = 1, nrow = 2, top = "Plot of the Raw Data")

    p

}
