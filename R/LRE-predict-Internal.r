#' @name LRE-predict-Internal
#' 
#' @title Internal functions for \code{predict.fitmodel} function.
#'
#' @description Internal functions used for prediction.
#' EstChat: estimates concentration.
#' BlockSigma: estimates the variance-covariance matrix.
#' CalcSE: calls on C code to calculate the standard error.
#' ConvertFacts: Internal function that determines the conversion
#' constant to use for loads and flow.Conversion to Million tonnes:
#' C = mg/L  Q = m^3/s (K.loads) mg/l x m3/1000 = 1kg  & 1kg = 0.001 t
#' Conversion to millions of ML (K.flow).
#' EstLoadYS: Function used in \code{predict.fitmodel} to estimate the
#' load based on predicted concentrations from predicted hourly flow records.
#' EstLoad: Function called by \code{EstLoadYS}
#' ExtractDailyLoads: extracts the daily loads.
#' Intervals: calculates confidence intervals.
#' 
#' @param est Estimation of response from model
#' @param Xdesign Design matrix from model
#' @param LhatC Estimated concentrations
#' @param alpha1 error in flows 
#' @param alpha2 error in flows 
#' @param beta1 beta1
#' @param rho correlation term
#' @param W W matrix
#' @param sig2 error
#' @param samp.unit sampling unit
#' @param x x
#' @param Conc Concentration
#' @param scale scale
#' @param pvalue pvalue
#' @param len len
#' @param Qdat Qdat
#' @param modobj modobj
#' @param modobjfix modobjfix
#' @param CQdat CQdat
#' @param se se
#' @param Blocklen Blocklen
#' @param concY concY
#' @param Date Date
#' @param Lhat Lhat
#' @param Qhat Qhat
#' @param error error
#' @param modparms modparms
#'
#' @import ggplot2 gridExtra
#' @importFrom stats qnorm na.omit coef vcov
#' @importFrom mgcv predict.gam
#' @importFrom rlang .data
#' 
#' @aliases Intervals calcSE BlockSigma ConvertFacts EstChat EstLoadYS ExtractDailyLoads LoadPlot
#' @keywords internal
NULL

#' @keywords internal EstLoad
Intervals <- function(est, pvalue){

  qn <- qnorm(1-pvalue/2)

  # Compute 95% CIs : annual
  est$loadest.annual$CI.low <- with(est$loadest.annual, TLhatC_Mt/exp(qn*CV))
  est$loadest.annual$CI.high <- with(est$loadest.annual, TLhatC_Mt*exp(qn*CV))
  # Compute 95% CIs : daily
  est$loadest.daily$CI.low <- with(est$loadest.daily, DLhatC_Mt/exp(qn*CV))
  est$loadest.daily$CI.high <- with(est$loadest.daily, DLhatC_Mt*exp(qn*CV))

  # Omit the uncorrected load estimate
  est$loadest.annual <- est$loadest.annual[,-3]
  est$loadest.daily <- est$loadest.daily[,-3]

  # Calculate Avg. Concentration:  TLhatC_Mt/Total_Flow_Ml  in mg/L
  ### annual
  est$loadest.annual$AvgConc <- with(est$loadest.annual, TLhatC_Mt/Total_Flow_ML * 1000000000)
  est$loadest.annual$AvgConcSE <- with(est$loadest.annual, sqrt((1000000000/Total_Flow_ML)^2 * SEload^2))
  est$loadest.annual$AvgConcCI.low <- with(est$loadest.annual, AvgConc/exp(qn*CV))
  est$loadest.annual$AvgConcCI.high <- with(est$loadest.annual, AvgConc*exp(qn*CV))

  ### daily
  est$loadest.daily$AvgConc <- with(est$loadest.daily, DLhatC_Mt/D_Flow_ML * 1000000000)
  est$loadest.daily$AvgConcSE <- with(est$loadest.daily, sqrt((1000000000/D_Flow_ML)^2 * SEload^2))
  est$loadest.daily$AvgConcCI.low <- with(est$loadest.daily, AvgConc/exp(qn*CV))
  est$loadest.daily$AvgConcCI.high <- with(est$loadest.daily, AvgConc*exp(qn*CV))

  est

}


#' @keywords internal
#' @useDynLib LRE, .registration = TRUE
CalcSE <-function(Xdesign, LhatC, alpha1, alpha2, beta1, rho, W, sig2){
  
  if(!is.loaded("calcSE")) dyn.load("calcSE.dll")
  
  len <- ncol(Xdesign)
  n <- nrow(Xdesign)
  storage.mode(Xdesign) <- storage.mode(LhatC) <- storage.mode(alpha1) <- "double"
  storage.mode(alpha2) <- storage.mode(beta1) <- storage.mode(rho) <- "double"
  storage.mode(W) <- storage.mode(sig2) <- "double"
  storage.mode(n) <- storage.mode(len) <- "integer"
  
  .C("calcSE", Xdesign, LhatC, alpha1 = alpha1, alpha2 = alpha2, beta1 = beta1,
     rho, W, sig2, len, n, SEload = double(1))$SEload
  
}

#' @keywords internal
BlockSigma <- function(len, Xdesign, W){
  
  n <- nrow(Xdesign)
  nB <- trunc(n/len)
  Sigma <- list()
  for(i in 1:nB){
    start <- (i-1)*len + 1
    stop <- i*len
    Sigma[[i]] <- diag(Xdesign[start:stop,] %*% W %*%
                         t(Xdesign[start:stop,]))
  }
  start <- nB*len+1
  stop <- n
  Sigma[[nB+1]] <- diag(Xdesign[start:stop,] %*% W %*%
                          t(Xdesign[start:stop,]))
  
  Sigma
  
}

#' @keywords internal
ConvertFacts <- function(samp.unit){
  
  if(samp.unit == "day"){
    K.loads <- 24*60*60/1000000/1000000 #Mt
    K.flow <- 24*60*60/1000
  }
  else if(samp.unit == "hour"){ # mg/l x m3/1000 = 1kg
    K.loads <-  60*60/1000000/1000000
    K.flow <- 60*60/1000
  }
  else stop("Sampling unit not implemented. Only day and hour are implemented.\n")
  
  list(K.loads = K.loads, K.flow = K.flow)
}


#' @keywords internal
EstChat <- function(Qdat, modobj, modobjfix){
  
  
  propOR <- 0  ; ratioMAXor <- 0 ; ratioMINor <- 0
  # now do the prediction
  
  if(length(modobj) == 2){
    # AR1 model
    
    W <- modobj$gam$Vp
    sig2 <- modobj$gam$sig2
    x <- modobj$lme$model$corStruct
    
    Xdesign <- predict(modobj$gam, newdata = Qdat, type = "lpmatrix")
    Cpred <- list(fit = predict(modobj$gam, newdata = Qdat, type = "response"),
                  se.fit = sqrt(rowSums((Xdesign %*% vcov(modobj$gam)) * Xdesign)))
    
    
    if(is.null(x))
      rho <- 0
    else
      rho <- coef(x, unconstrained = FALSE)
  }
  else
    if(inherits(modobj, "bam")){
      if(length(coef(modobj)) == 1){
        # Constant model
        Cpred <- predict(modobj, type = "response", se.fit = TRUE)
        Cpred$fit <- rep(Cpred$fit[1], nrow(Qdat))
        Cpred$se.fit <- rep(Cpred$se.fit[1], nrow(Qdat))
        
        # Get design matrix, Var(betahat), sigma^2
        Xdesign <- data.frame(Intercept = rep(1, nrow(Qdat)))
      }
      else{
        Cpred <- predict(modobj, Qdat, type = "response", se.fit = TRUE)
        # Get design matrix, Var(betahat), sigma^2
        Xdesign <- predict(modobj, newdata = Qdat, type = "lpmatrix")
      }
      
      W <- modobj$Vp
      sig2 <- modobj$sig2
      rho <- modobj$AR1.rho
    }
  else{
    
    if(length(coef(modobj)) == 1){
      # Constant model
      Cpred <- predict(modobj, type = "response", se.fit = TRUE)
      Cpred$fit <- rep(Cpred$fit[1], nrow(Qdat))
      Cpred$se.fit <- rep(Cpred$se.fit[1], nrow(Qdat))
      
      # Get design matrix, Var(betahat), sigma^2
      Xdesign <- data.frame(Intercept = rep(1, nrow(Qdat)))
    }
    else{
      Cpred <- predict(modobj, Qdat, type = "response", se.fit = TRUE)
      # Get design matrix, Var(betahat), sigma^2
      Xdesign <- predict(modobj, newdata = Qdat, type = "lpmatrix")
    }
    
    W <- modobj$Vp
    sig2 <- modobj$sig2
    rho <- 0
  }
  
  
  Clhat <- Cpred$fit    # predicted concentration on log scale
  Cpred.SE <- Cpred$se.fit
  Qhat <- Qdat$pQ     # predicted flow
  
  CQhat <- data.frame(Date = Qdat$Date, Y = Qdat$Y, Chat = exp(Clhat), ChatSE = Cpred.SE, Qhat = Qhat)
  
  list(CQhat = CQhat, Xdesign = Xdesign, W = W, sig2 = sig2, rho = rho,
       propOR = propOR, ratioMAXor = ratioMAXor, ratioMINor = ratioMINor)
}


#' @keywords internal
EstLoad <- function(CQdat, Qdat, Xdesign, W, sig2, rho,
                    alpha1, alpha2, beta1, samp.unit, se = TRUE, Blocklen = 5000){
  
  
  Chat <- CQdat$Chat
  Qhat <- CQdat$Qhat
  
  # Estimate the uncorrected load
  
  
  ##### Calculate the correction factor and corrected Load
  
  if(nrow(Xdesign) < Blocklen)
    Sigma <- diag(Xdesign %*% W %*% t(Xdesign))
  else
    Sigma <- BlockSigma(len = Blocklen, Xdesign = Xdesign, W = W)
  Sigma <- unlist(Sigma)
  cf <- numeric(length(Sigma))
  for(i in 1:length(Sigma))
    cf[i] <- exp((sig2-Sigma[i])/2)
  
  LhatC <- Chat*Qhat*cf
  Lhat <- Chat*Qhat
  
  ##### Calculate SE
  if(se)
    SEload <- CalcSE(Xdesign, LhatC, alpha1, alpha2, beta1, rho, W, sig2)
  else
    SEload <- NA
  
  daily.est <- ExtractDailyLoads(Date = CQdat$Date, Xdesign = Xdesign, Lhat = Lhat,
                                 LhatC = LhatC, Qhat = Qhat, error = list(alpha1=alpha1, alpha2=alpha2, beta1=beta1),
                                 modparms=list(rho=rho, W=W, sig2=sig2))
  
  
  Cfactors <- ConvertFacts(samp.unit)
  # Loads Estimates
  Lhat <- Chat*Qhat * Cfactors$K.loads
  LhatC<- LhatC * Cfactors$K.loads
  SEload <- SEload * Cfactors$K.loads
  Lhat.day <- with(daily.est, Lhat.day * Cfactors$K.loads)
  LhatC.day <- with(daily.est, LhatC.day * Cfactors$K.loads)
  Qhat.day <- with(daily.est, Qhat.day * Cfactors$K.flow)
  SEload.day <- with(daily.est, SEload.day * Cfactors$K.loads)
  TotalFlow <- sum(Qhat*Cfactors$K.flow)
  
  est.annual <- data.frame(Y = unique(CQdat$Y), Total.Flow = TotalFlow,
                           TLhat = sum(Lhat), TLhatC = sum(LhatC),
                           SEload = SEload, CV = SEload/sum(LhatC))
  
  est.daily <- data.frame(Year = unique(CQdat$Y), Y = daily.est$Date, Lhat = Lhat.day, LhatC = LhatC.day,
                          Qhat = Qhat.day, SEload = SEload.day, CV = SEload.day/LhatC.day)
  
  list(est.annual = est.annual, est.daily = est.daily)
}


#' @keywords internal
EstLoadYS <- function(CQdat, Qdat, concY, modobj, samp.unit = samp.unit,
                      alpha1, alpha2, se = TRUE, Blocklen=5000){
  
  # samp.unit = sampling unit the predicted flow is at
  
  uY <- unique(CQdat$CQhat$Y)
  loadest <- matrix(NA, nrow = length(uY), ncol = 6)
  loadest <- data.frame(loadest)
  names(loadest) <-  c("Y", "Total_Flow_ML", "TLhat_Mt", "TLhatC_Mt", "SEload", "CV")
  
  loadest.d <- NULL
  for(i in 1:length(uY)){
    cat("Year: ", uY[i], "\n")
    CQhatY <- CQdat$CQhat[CQdat$CQhat$Y == uY[i],]
    
    
    QdatY <- Qdat[Qdat$Y == uY[i],]
    
    Xdesign <- as.matrix(CQdat$Xdesign[Qdat$Y == uY[i],])
    
    if(length(modobj) == 2){
      coef1 <- coef(modobj$gam)[pmatch("LQflow", names(coef(modobj)))]
      if(length(coef1) == 1)
        coef2 <- NA
      else{
        coef2 <- coef1[2]
        coef1 <- coef1[1]
      }
    }
    else
    {
      coef1 <- coef(modobj)[pmatch("LQflow", names(coef(modobj)))]
      if(length(coef1) == 1)
        coef2 <- NA
      else{
        coef2 <- coef1[2]
        coef1 <- coef1[1]
      }
    }
    
    if(is.na(coef1) & is.na(coef2))
      beta1 <- 0  # no flow terms fitted
    else if(is.na(coef2) & !is.na(coef1))
      beta1 <- coef1
    else
      beta1 <- coef1 + 2 * coef2 * log(QdatY$pQ)
    
    
    est <- EstLoad(CQdat = CQhatY, Qdat = QdatY, Xdesign = Xdesign,
                   W = CQdat$W, sig2 = CQdat$sig2, rho = CQdat$rho, alpha1 = alpha1,
                   alpha2 = alpha2, beta1 = beta1,
                   samp.unit = samp.unit, se = TRUE)
    
    loadest[i,] <- est$est.annual
    loadest.d <- rbind(loadest.d, est$est.daily)
    
  }
  
  names(loadest.d) <- c("Y", "Date", "DLhat_Mt", "DLhatC_Mt", "D_Flow_ML", "SEload", "CV")
  # Add Y
  loadest$Y <- uY
  # Add number of recorded concentration samples
  
  tabY <- table(concY)
  ind <- match(names(tabY), as.vector(loadest$Y))
  loadest$n <- 0
  loadest$n[ind[!is.na(ind)]] <- tabY[!is.na(ind)]
  list(loadest.annual = loadest, loadest.daily = loadest.d)
}


#' @keywords internal
ExtractDailyLoads <- function(Date, Xdesign, Lhat, LhatC, Qhat, error, modparms){
  
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

#' @keywords internal
LoadPlot <- function(x, Conc, scale = "Mt"){
    
    # Annual Loads
    
    pF1 <- ggplot(aes(x = .data[["Date"]], y = .data[["flow"]]), data = x) + 
      geom_bar(stat = "identity") + coord_flip()
    pF2 <- pF1 + ylab("Total Flow (ML)") + theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
                                                 axis.ticks.y = element_blank())
    
    if(scale == "Mt"){
      pC1 <- ggplot(aes(x = .data[["y"]], y = .data[["Date"]]), data = x) + geom_point() +
        geom_errorbarh(aes(xmin = .data[["CI.low"]], xmax = .data[["CI.high"]]), size = 1) +
        ylab("") + xlab(paste(Conc, "(Mt)", sep = ""))
      pL1 <- marrangeGrob(list(pC1, pF2), nrow = 1, ncol = 2, widths = c(3,1), top = "Load Estimates (Mt)")
      
    }
    else if(scale == "t"){
      x$yt <- x$y*1000000
      x$CI.lowt <- x$CI.low*1000000
      x$CI.hight <- x$CI.high*1000000
      pC1 <- ggplot(aes(x = .data[["yt"]], y = .data[["Date"]]), data = x) + geom_point() +
        geom_errorbarh(aes(xmin = .data[["CI.lowt"]], xmax = .data[["CI.hight"]]), size = 1) +
        ylab("") + xlab(paste(Conc, "(t)", sep = ""))
      pL1 <- marrangeGrob(list(pC1, pF2), nrow = 1, ncol = 2, widths = c(3,1), top = "Load Estimates (t)")
      
    }
    else
      stop("Scale can be either Mt or t. \n")
    
    
    # Flow weighted concentrations
    
    pFWC1 <- ggplot(aes(x = .data[["Date"]], y = .data[["AvgConc"]]), data = x) + 
      geom_point() +
      geom_errorbar(aes(ymin = .data[["AvgConcCI.low"]], ymax = .data[["AvgConcCI.high"]]), 
                    size = 1, width = 0.4) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + xlab("WY") +
      ylab("Flow weighted concentrations (mg/L)") + ggtitle(Conc)
    
    
    
    list(pL1 = pL1, pFWC1 = pFWC1)
    
  }




