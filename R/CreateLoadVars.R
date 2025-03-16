#' CreateLoadVars
#' 
#' LRE Internal function which create load features
#' 
#' @param Q Flow
#' @param csQ Cumulative sum of flow
#' @param Qflush quantile associated with first flush
#' @param samp.unit sampling unit
#' @param Date Date vector
#' @param Y Year type
#'
#'
#' @importFrom TTR EMA
CreateLoadVars <- function(Q, csQ, Qflush, samp.unit, Date, Y){


 # Identify flush
 qY <- data.frame(qpc = tapply(Q, Y, function(x) quantile(x, Qflush)))
 qY$Y <- row.names(qY)
 row.names(qY) <- NULL
 id <- match(Y, qY$Y)
 qpc <- qY$qpc[id]

 # Determine where there is a big flow
 Bflow <- ifelse(Q <= qpc, 0, log(Q))

 # Calculate the limb: 0=no flush; 1= flush in rising limb; -1 = flush in decreasing limb
 d <- c(0, diff(Q))
 limb <- (Q > qpc) * sign(d)
 df <- data.frame(Date = Date, Q = Q, Limb = limb)
 p1 <- ggplot(df, aes_string('Date', 'Q')) + geom_line() + ylab("Flow")
 p2 <- ggplot(df, aes_string('Date', 'Limb'), col = "blue") + geom_line()
 p_flush <- marrangeGrob(list(p1, p2), ncol = 1, nrow = 2, top = "Rising/Falling Limb")



 # Calculate MA terms
 if(samp.unit == "hour"){

    # past 24 hours, past 2 days, past week, past month, past 3 months, past 6 months, past 12 months
    discount <- c(24, 24*2, 24*7, 24*30, 24*30*3, 24*30*6, 24*30*12)
    ema.sm <- sapply(discount, function(x) EMA(Q, n = x))
    # now replace NAs with MA of values up to n, where n is the largest n used
    replNA <- unlist(lapply(1:(24*30*12), function(x){ tmp <- EMA(Q, n = x)
                                         tmp[!is.na(tmp)][1]}))
    for(i in 1:ncol(ema.sm)){
      ema.sm[,i][is.na(ema.sm[,i])] <- replNA[is.na(ema.sm[,i])]
    }


 }
 else if(samp.unit == "day"){
   # past day, past 2 days, past week, past month, past 3 months, past 6 months, past 12 months
   discount <- c(1, 2, 7, 30, 30*3, 30*6, 30*12)
   ema.sm <- sapply(discount, function(x) EMA(Q, n = x))
   # now replace NAs with MA of values up to n, where n is the largest n used
   replNA <- unlist(lapply(1:(30*12), function(x){ tmp <- EMA(Q, n = x)
                                            tmp[!is.na(tmp)][1]}))
   for(i in 1:ncol(ema.sm)){
     ema.sm[,i][is.na(ema.sm[,i])] <- replNA[is.na(ema.sm[,i])]
   }


 }
 else stop("The sampling unit specified has not been implement. Choose either 'day' or 'hour'.")


 dframe <- data.frame(ema.sm)
 nms <- paste("MA", c("1day", "2days", "week", "month", "3months", "6months", "12months"), sep = "")
 names(dframe) <- nms



   dat <- data.frame(Q = Q, ema.sm, Date = Date)
   names(dat) <- c("Q", nms, "Date")
   p1 <- ggplot(data = dat, aes(Date, Q)) + geom_line(col = "black")
   p2 <- p1 + geom_line(data = dat, aes_string('Date', 'MA1day'), col = "red")
   p3 <- p2 + geom_line(data = dat, aes_string('Date', 'MA2days'), col = "orange")
   p4 <- p3 + geom_line(data = dat, aes_string('Date', 'MAweek'), col = "yellow")
   p5 <- p4 + geom_line(data = dat, aes_string('Date', 'MAmonth'), col = "green")
   p6 <- p5 + geom_line(data = dat, aes_string('Date', 'MA3months'), col = "lightblue")
   p7 <- p6 + geom_line(data = dat, aes_string('Date', 'MA6months'), col = "blue")
   p_lagterms <- p7 + geom_line(data = dat, aes_string('Date', 'MA12months'), col = "purple")



list(Flush = qpc, limb = limb, dcum = dframe, p_lagterms = p_lagterms, p_flush = p_flush)

}

