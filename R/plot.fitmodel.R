#' @name plot.fitmodel
#'
#' @title Plot fitmodel objects
#'
#' @description Plot object of class 'fitmodel'
#'
#' @param x data object
#' @param Qreg regularised flow
#' @param data data object
#' @param ... other parameters passed to \code{plot}
#'
#' @importFrom "visreg" "visreg"
#' @importFrom "stats" "model.matrix"
#' @import "ggplot2"
#' @import "gridExtra"
#' @import "dplyr"
#'
#' @export
plot.fitmodel <- function(x, Qreg, data, ...){

  if(class(x)[1] != "fitmodel")
    stop("Object is not of class 'fitmodel'.\n")

  if(missing(Qreg))
    stop("Regularised flow (Qreg) has not been supplied.\n")

  if(missing(data))
    stop("Data object has not been supplied.\n")

  ##########################################
  # Preliminaries
  ##########################################
  if(length(x) == 2){
     term.pred <- predict(x$gam, Qreg, type = "terms", se.fit = TRUE)
     modelfit <- x$gam
     modelfit$data <- data
  }
  else{
 
     term.pred <- predict(x, Qreg, type = "terms", se.fit = TRUE)
     modelfit <- x
     class(modelfit) <- class(modelfit)[-1]
     
  }

  ##########################################
  # Termplots
  ##########################################


  # Plot 1

  # Rising/Falling Limb
  id <- match("limb", names(data.frame(term.pred$fit)))
  if(any(!is.na(id))){

     limb.est <- tapply(term.pred$fit[,id], Qreg$limb, function(x) na.omit(unique(x)))
     limb.est <- limb.est[names(limb.est) != 0]
     limb.se <- tapply(term.pred$se.fit[,id], Qreg$limb, function(x) na.omit(unique(x)))
     limb.se <- limb.se[names(limb.se) != 0]
     limb.95lci <- limb.est - 1.96 * limb.se
     limb.95uci <- limb.est + 1.96 * limb.se
     pcols <- ifelse(limb.95lci < 0 & limb.95uci > 0, "Not Significant", "Significant (0.05)")

     limb.df <- data.frame(x = names(limb.est), est = exp(limb.est),
                           l95lci = exp(limb.95lci), l95uci = exp(limb.95uci), Significance = pcols)

     p1 <- ggplot(aes_string(x = 'x', y = 'est', color = 'Significance'), data = limb.df) + geom_point(size = 4) +
       geom_errorbar(aes_string(ymax = 'l95uci', ymin = 'l95lci'), size = 1, width = 0.2)
     pRFL <- p1 + geom_hline(yintercept = 1, linetype = 2) +
       xlab("") + scale_color_manual(values = as.vector(ifelse(pcols == "Not Significant", "darkgrey", "black"))) +
       ggtitle("Comparison of Rising/Falling Limb to Baseline Levels") +
       ylab("Contribution to predicted concentration (mg/L)")
  }
  else
    pRFL <- NULL

     # Plot 2
     # flow terms
     id1 <- match("LQflow.pQ.", names(data.frame(term.pred$fit)))
     id2 <- match("LQflow.pQ..quad...TRUE.", names(data.frame(term.pred$fit)))
     id3 <- match("LQflow.pQ..quad...FALSE.", names(data.frame(term.pred$fit)))
     id <- na.omit(c(id1, id2, id3))
     if(any(!is.na(id))){
        # restrict to the range of the modelling dataset
        rangeID <- Qreg$pQ >= min(data$pQ) & Qreg$pQ <= max(data$pQ)
        flow.est <- term.pred$fit[,id][rangeID]
        flow.se <- term.pred$se.fit[,id][rangeID]
        flow.95lci <- flow.est - 1.96 * flow.se
        flow.95uci <- flow.est + 1.96 * flow.se
        lpQ <- log(Qreg$pQ)[rangeID]

        flow.df <- data.frame(x = sort(lpQ), est = flow.est[order(lpQ)], lci = flow.95lci[order(lpQ)],
                              uci = flow.95uci[order(lpQ)])

        
        p1 <- ggplot(aes_string('x', 'est'), data = flow.df) + xlab("log(Flow)") +
          ylab("Contribution to predicted concentration (log-scale)") + ggtitle("Flow term")
        pFlow <- p1 + geom_ribbon(aes_string(ymin = 'lci', ymax = 'uci'), fill = "grey70") + geom_line(size = 1)


     }
     else
       pFlow <- NULL


  ##########################################
  # Smooth Terms: Using plot.gam to plot smoothers
  ##########################################

   allterms <- attr(modelfit$terms, "term.labels")
  # Plot:  Seasonal
  id <- match("month", allterms)
  if(!(any(is.na(id)) | length(id) == 0)){
  
    p1 <- visreg(modelfit, "month", gg=TRUE, data = data) + 
      ylab("Contribution to predicted concentration (log-scale)") +
      xlab("Month (Oct - Sept)") + ggtitle("Seasonal Component")
    pSeas <- p1 + scale_x_continuous(breaks = seq(2, 12, by = 2), labels = c("Nov", "Jan", "Mar", "May", "Jul", "Sep"))
  }
  else
    pSeas <- NULL

  # Plot: MA terms
  id <- grep("MA", allterms)
 if(!(any(is.na(id)) | length(id) == 0)){
     sterms <- allterms[id]
     p <- list()
     for(i in 1:length(sterms))
       p[[i]] <- visreg(modelfit, sterms[i], gg = TRUE, data = data) + 
                  ylab(paste("s(", sterms[i], ")")) +
                  ggtitle(sterms[i])
     if(length(sterms) == 1)
        pMA <- p[[1]]
     else if(length(sterms) == 2)
        pMA <- marrangeGrob(p, nrow = 1, ncol = 2, top = "Smooth Flow Terms")
     else if(length(sterms) <= 4)
       pMA <- marrangeGrob(p, nrow = 2, ncol = 2, top = "Smooth Flow Terms")
     else if(length(sterms) <= 6)
       pMA <- marrangeGrob(p, nrow = 3, ncol = 2, top = "Smooth Flow Terms")
  }
  else
    pMA <- NULL


    # Plot 6: trend (years)
  id <- match("trendY", allterms)
  if(!(any(is.na(id)) | length(id) == 0)){
    pTrend <- visreg(modelfit, "trendY", gg = TRUE, data = data) + ylab("s(trend)") +
      ggtitle("Trend term") + xlab("Year")
  }
  else
    pTrend <- NULL


  ##########################################
  # Predictions
  ##########################################
  # modelling dataset
  if(length(x) == 2){
    Xdesign <- model.matrix(modelfit)
     yhat <- list(fit = predict(modelfit),
         se.fit = sqrt(rowSums((Xdesign %*% vcov(x$gam)) * Xdesign)))
  }
  else
     yhat <- predict(modelfit, se.fit = TRUE)

  yhat.l <- yhat$fit - 1.96 * yhat$se.fit
  yhat.u <- yhat$fit + 1.96 * yhat$se.fit
  yhat <- yhat$fit

  # check for NAs which may occur with the moving averages
#  any.na <- apply(data, 1, function(x) any(is.na(x)))
  
#  predmat <- data.frame(Date = data$Date[!any.na], Y = data$Y[!any.na], 
 #                       Conc = data$Conc[!any.na], yhat = yhat,
#                      yhat.l = yhat.l, yhat.u = yhat.u, pQ = data$pQ[!any.na])
#
  predmat <- data.frame(Date = data$Date, Y = data$Y, 
                                               Conc = data$Conc, yhat = yhat,
                        yhat.l = yhat.l, yhat.u = yhat.u, pQ = data$pQ)
  
  # regularised dataset
  if(length(x) == 2){
    Xdesign <- predict(modelfit, newdata = Qreg, type = "lpmatrix")
    yhatR <- list(fit = predict(modelfit, newdata = Qreg),
         se.fit = sqrt(rowSums((Xdesign %*% vcov(x$gam)) * Xdesign)))

  }
  else
     yhatR <- predict(modelfit, Qreg, se.fit = TRUE)

  yhatR.l <- yhatR$fit - 1.96 * yhatR$se.fit
  yhatR.u <- yhatR$fit + 1.96 * yhatR$se.fit
  yhatR <- yhatR$fit
  predmatR <- data.frame(Date = Qreg$Date, Y = Qreg$Y,  yhat = yhatR,
                       yhat.l = yhatR.l, yhat.u = yhatR.u, pQ = Qreg$pQ)

  predmatO <- predmat[,c(1,2,3,5:7)]
  names(predmatO)[3] <- "yhat"
  predmatO$yhat <- log(predmatO$yhat)
  predmatC <- rbind(predmatR, predmat[,-3], predmatO)
  predmatC$Concentration <- c(rep("Regularised", nrow(predmatR)), rep("Monitoring", nrow(predmat)), rep("Observed", nrow(predmatO)))

  xlabel <- unique(data$Y)
  conc_mon <- predmatC[predmatC$Concentration == "Monitoring" | predmatC$Concentration == "Observed",]
  conc_reg <- predmatC[predmatC$Concentration == "Regularised",]

  pConc <- ggplot(aes(Date, yhat, colour = Concentration), data = predmatC) +
    geom_point(aes(Date, yhat), data = conc_mon) +
    geom_line(aes(Date, yhat), data = conc_reg) +
        scale_color_manual(values = c("green3", "orange2", "blue")) + ylab("log(Concentration)") + xlab("") +
    theme(legend.position="top")
  predmatC$lpQ <- log(predmatC$pQ)
  pFlow <- ggplot(aes(Date, lpQ), data = predmatC) + geom_line() + xlab(paste(xlabel[1], " to ",
                                                                                      xlabel[length(xlabel)])) + ylab("log(Flow_R)")
  ppred <- marrangeGrob(list(pConc,pFlow), nrow = 2, ncol = 1, heights = c(2,1), top = "Predicted Time Series Concentration")

  #  with error bands
  # Filter data for regularised estimates
  df_regularised <- predmatC %>% filter(Concentration == "Regularised")
  
  # Plot
  pConcInt <- ggplot(predmatC, aes(x = Date)) +
    # Line for yhat
    geom_line(aes(y = yhat), color = "blue", linewidth = 1) +
    # Error bands for Regularised estimates
    geom_ribbon(data = df_regularised, aes(ymin = yhat.l, ymax = yhat.u), 
                fill = "blue", alpha = 0.2) +
    # Points for Observed
    geom_point(data = predmatC %>% filter(Concentration == "Observed"), 
               aes(y = yhat, color = "Observed"), size = 1) +
    scale_color_manual(values = c("Monitoring" = "red", "Observed" = "green")) +
    labs(x = "", y = "log(Concentration)", title = "Predicted Time Series Concentrations and Uncertainties with observed") +
    theme_minimal()
  

    list(pTrend = pTrend, pMA = pMA, pSeas = pSeas, pRFL = pRFL, ppred = ppred, pConc = pConc,
         pConcInt)



}

