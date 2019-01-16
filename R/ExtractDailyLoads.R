#' @importFrom "stats" "na.omit"
#' @rdname LRE-predict-Internal
ExtractDailyLoads <-function(Date, Xdesign, Lhat, LhatC, Qhat, error, modparms){

    startdate <- min(Date)
    startdate <- as.POSIXlt(startdate)
    startdate$hour <- 0 ; startdate$min <- 0 ; startdate$sec <- 0
    startdate <- as.POSIXct(startdate)
    d <- difftime(Date, startdate, units = "days")
    d.round <- trunc(d)+1

    # Grab unique dates (days)
    dd <- as.POSIXlt(Date)
    dd$hour <- 0 ; dd$min <- 0 ; dd$sec <- 0
    un.days <- unique(as.POSIXct(dd))
    un.days <- un.days[!is.na(un.days)]

    # Daily estimates
    Lhat.day <- tapply(Lhat, d.round, sum)
    LhatC.day <- tapply(LhatC, d.round, sum)
    Qhat.day <- tapply(Qhat, d.round, sum)

    un.d <- unique(d.round)
    SEload.day <- numeric(length(un.d))

    for(i in 1:length(un.d)){
     # cat("i = ", i,"\n")
      if(table(d.round)[i] > 1)
         if(length(error$beta1) == 1)
            SEload.day[i] <- CalcSE(Xdesign[d.round == un.d[i],], LhatC[d.round == un.d[i]],
                         error$alpha1, error$alpha2, error$beta1,
                         modparms$rho, modparms$W, modparms$sig2)
         else
            SEload.day[i] <- CalcSE(Xdesign[d.round == un.d[i],], LhatC[d.round == un.d[i]],
                         error$alpha1, error$alpha2, na.omit(error$beta1[d.round == un.d[i]]),
                         modparms$rho, modparms$W, modparms$sig2)
    }

    data.frame(Date = un.days, Lhat.day = Lhat.day, LhatC.day = LhatC.day,
                Qhat.day = Qhat.day, SEload.day = SEload.day)

}

