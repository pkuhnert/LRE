#' PlotCQ
#' 
#' LRE Internal function
#' 
#' @param Conc Concentration
#' @param Q Flow
#'
PlotCQ <- function(Conc, Q){

  Conc$lConc <- log(Conc$Conc)
  Conc$lpQ <- log(Conc$pQ)
  p1 <- ggplot(aes(Date, lConc), data = Conc) + geom_line() + ylab("log(Concentration)")
  p2 <- ggplot(aes(Date, lpQ), data = Conc) + geom_line() + ylab("log(Flow)")

  minD <- min(Conc$Date)
  maxD <- max(Conc$Date)
  id <- (Q$Date >= minD & Q$Date <= maxD)
  Qsub <- Q[id,]
  Qsub$lpQ <- log(Qsub$pQ)
  p3 <- ggplot(aes(Date, lpQ), data = Qsub) + geom_line(colour = "blue")

  p <- marrangeGrob(list(p1, p2, p3), ncol = 1, nrow = 3, top = "Relationship between concentration and flow")


  p
}

