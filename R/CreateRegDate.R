#' CreateRegDate
#' 
#' LRE Internal function.
#' 
#' @param Q Flow 
#' @param samp.unit sampling unit
#' @param Ytype Year type: water year ("WY" or "WY2"), financial year ("FY")
#'
CreateRegDate <- function(Q, samp.unit, Ytype){

  start.date <- min(Q$Date)
  end.date <- max(Q$Date)

 if(samp.unit == "hour")
    len <- 60*60
 else if(samp.unit == "day")
    len <- 60*60*24
 else stop("sampling unit not defined\n")

 d <- seq(start.date, end.date, by = len)

 Qreg <- data.frame(Date = d)
 days <- with(Qreg, as.numeric(difftime(Date, start.date, units = "days")))
 Y <- CreateY(d, Ytype = Ytype)


 data.frame(Date = d, days = days, Y = Y)


}

