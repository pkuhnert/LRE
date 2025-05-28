#' @name plot.fitmodel
#'
#' @title Plot predictions from the fitted model
#'
#' @description Plot object of class 'fitmodel'
#'
#' @param x data object
#' @param Qreg Regularised flow record
#' @param data dataset that was used for modelling
#' @param ... other parameters passed to \code{plot}
#'
#' @import ggplot2
#' @import gridExtra
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom stats model.matrix
#' @importFrom rlang .data
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
   # modelfit$data <- data
  }
  else{
    
    term.pred <- predict(x, Qreg, type = "terms", se.fit = TRUE)
    modelfit <- x
    class(modelfit) <- class(modelfit)[-1]
    
  }
  

  
  
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
  
  pConc <- ggplot(aes(x = .data[["Date"]], y = .data[["yhat"]], colour = .data[["Concentration"]]), 
                  data = predmatC) +
    geom_point(aes(x = .data[["Date"]], y = .data[["yhat"]]), data = conc_mon) +
    geom_line(aes(x = .data[["Date"]], y = .data[["yhat"]]), data = conc_reg) +
    scale_color_manual(values = c("green3", "orange2", "blue")) + ylab("log(Concentration)") + xlab("") +
    theme(legend.position="top")
  predmatC$lpQ <- log(predmatC$pQ)
  pFlow <- ggplot(aes(x = .data[["Date"]], y = .data[["lpQ"]]), data = predmatC) + geom_line() + 
    xlab(paste(xlabel[1], " to ",  xlabel[length(xlabel)])) + ylab("log(Flow_R)")
  ppred <- marrangeGrob(list(pConc,pFlow), nrow = 2, ncol = 1, heights = c(2,1), top = "Predicted Time Series Concentration")

  #  with error bands
  # Filter data for regularised estimates
  df_regularised <- predmatC %>% filter(.data[["Concentration"]] == "Regularised")

  # Plot
  pConcInt <- ggplot(predmatC, aes(x = .data[["Date"]])) +
    # Line for yhat
    geom_line(aes(y = .data[["yhat"]]), color = "blue", linewidth = 1) +
    # Error bands for Regularised estimates
    geom_ribbon(data = df_regularised, aes(ymin = .data[["yhat.l"]], ymax = .data[["yhat.u"]]), 
                fill = "blue", alpha = 0.2) +
    # Points for Observed
    geom_point(data = predmatC %>% filter(.data[["Concentration"]] == "Observed"), 
               aes(y = .data[["yhat"]], color = "Observed"), size = 1) +
    scale_color_manual(values = c("Monitoring" = "red", "Observed" = "green")) +
    labs(x = "", y = "log(Concentration)", title = "Predicted Time Series Concentrations and Uncertainties with observed") +
    theme_minimal()
  
  

  list(ppred = ppred, pConc = pConc, pConcInt = pConcInt)
  


}
