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
#' @param Qdat Qdat
#' @param modobj modobj
#' @param modobjfix modobjfix
#' @param len len
#' @param Xdesign Design matrix
#' #' @param W Var(betahat)
#' @param sig2 sigma^2
#' @param rho rho
#' @param alpha1 measurement error
#' @param alpha2 error in flow rates
#' @param beta1 coeficient of log Q in the regression
#' @param samp.unit sampling unit
#' @param se se (Default: TRUE)
#' @param Blocklen Block length (Default: 5000)
#' @param LhatC Concentration estimate
#' @param CQdat Predicted concentration and flow
#' @param concY concY
#' @param Date Date
#' @param Lhat Load estimate
#' @param Qhat Flow estimate
#' @param error error
#' @param modparms modparms
#' @param est estimate
#' #' @param pvalue pvalue
#'
#' @importFrom "stats" "predict" "vcov" "coef"
#'
NULL


#' @rdname LRE-predict-Internal
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

