#' @name plot.PredictLoad
#'
#' @title plot.PredictLoad
#'
#' @description Plots load estimates from the fitted model
#'
#'
#' @param x object
#' @param type type of load (annual or daily)
#' @param Conc Concentration label
#' @param newQ new flow vector (Default: NULL)
#' @param scale scale to plot loads (Default: Mt)
#' @param ... other parameters passed to \code{plot}
#'
#'
#' @references Kuhnert, P.M., Henderson, B.L., Lewis, S.E.,
#' Bainbridge, Z.T., Wilkinson, S.N. and Brodie, J.E. (2012)
#' Quantifying total suspended sediment export from the Burdekin
#' River catchment using the loads regression estimator tool,
#' Water Resources Research, 48, W04533,doi:10.1029/2011WR011080.
#'
#'
#' @keywords loads, metrics
#'
#' @export

plot.PredictLoad <- function(x, type = "annual", Conc, newQ = NULL, scale = "Mt", ...){

  if(class(x) != "PredictLoad")
    stop("Object is not of class 'PredictLoad'.\n")
  if(missing(Conc))
    stop("Please provide a Concentration label (Conc)\n.")
  if(!(scale == "Mt" | scale == "t"))
    stop("Scale can either be 'Mt' or 't'.\n")


   # Plotting
   if(type == "annual"){
      x <- x$annual
      y <- x$TLhatC_Mt
      y.lci <- x$CI.low
      y.uci <- x$CI.high
      if(is.null(newQ))
         flow <- x$Total_Flow_ML
      else{
         flow <- newQ
         flow <- as.numeric(flow)
      }
      yC <- x$AvgConc
      yC.lci <- x$AvgConcCI.low
      yC.uci <- x$AvgConcCI.high
      ydf <- data.frame(Date = x$Y, y=y, CI.low = y.lci, CI.high = y.uci, flow = flow, AvgConc = yC, AvgConcCI.low = yC.lci,
                        AvgConcCI.high = yC.uci)


      p <- LoadPlot(ydf, Conc, scale)
   }
   else if(type == "daily"){

     x <- x$daily
     u.id <- unique(x$Y)
     for(i in 1:length(u.id)){
         x.sub <- x[x$Y == u.id[i],]
         y <- x.sub$DLhatC_Mt
         y.lci <- x.sub$CI.low
         y.uci <- x.sub$CI.high
         flow <- x.sub$D_Flow_ML
         yC <- x.sub$AvgConc
         yC.lci <- x.sub$AvgConcCI.low
         yC.uci <- x.sub$AvgConcCI.high
         Year <- x.sub$Y
         ydf <- list(Date = x.sub$Date, Year = Year, y = y, CI.low = y.lci, CI.high = y.uci, flow = flow,
                        AvgConc = yC, AvgConcCI.low = yC.lci, AvgConcCI.high = yC.uci)
         p <- LoadPlot(ydf, Conc, scale)
     }
   }
   else
      stop("Incorrect type specified. Only annual or daily loads implemented.\n")

  p
}


