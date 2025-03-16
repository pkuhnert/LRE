#' @rdname LRE-predict-Internal
EstLoad <- function(CQdat, Qdat, Xdesign, W, sig2, rho,
                 alpha1, alpha2, beta1, samp.unit, se = TRUE, Blocklen=5000){


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
