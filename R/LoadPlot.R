#' @name LRE-plotpredict-Internal
#'
#' @title Internal function used to plot predictions in the
#' \code{plot.PredictLoad} function.
#'
#' @description Function that calculates confidence intervals
#'
#' @param x x
#' @param Conc Concentration
#' @param scale scale
#'
#' @import "ggplot2"
#' @import "gridExtra"
NULL

LoadPlot <-
function(x, Conc, scale = "Mt"){

    # Annual Loads

    pF1 <- ggplot(aes(Date, flow), data = x) + geom_bar(stat = "identity") + coord_flip()
    pF2 <- pF1 + ylab("Total Flow (ML)") + theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
                                                 axis.ticks.y = element_blank())

    if(scale == "Mt"){
      pC1 <- ggplot(aes(y, Date), data = x) + geom_point() +
              geom_errorbarh(aes(xmin = CI.low, xmax = CI.high), size = 1) +
        ylab("") + xlab(paste(Conc, "(Mt)", sep = ""))
      pL1 <- marrangeGrob(list(pC1, pF2), nrow = 1, ncol = 2, widths = c(3,1), top = "Load Estimates (Mt)")

    }
    else if(scale == "t"){
      x$yt <- x$y*1000000
      x$CI.lowt <- x$CI.low*1000000
      x$CI.hight <- x$CI.high*1000000
      pC1 <- ggplot(aes(yt, Date), data = x) + geom_point() +
        geom_errorbarh(aes(xmin = CI.lowt, xmax = CI.hight), size = 1) +
        ylab("") + xlab(paste(Conc, "(t)", sep = ""))
      pL1 <- marrangeGrob(list(pC1, pF2), nrow = 1, ncol = 2, widths = c(3,1), top = "Load Estimates (t)")

    }
    else
      stop("Scale can be either Mt or t. \n")


    # Flow weighted concentrations

    pFWC1 <- ggplot(aes(Date, AvgConc), data = x) + geom_point() +
                geom_errorbar(aes(ymin = AvgConcCI.low, ymax = AvgConcCI.high), size = 1, width = 0.4) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("WY") +
                ylab("Flow weighted concentrations (mg/L)") + ggtitle(Conc)



      list(pL1 = pL1, pFWC1 = pFWC1)

}

