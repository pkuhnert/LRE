#' @name extract_trend
#'
#' @title Extracts the trend 
#'
#' @description Extracts the trend and 95% confidence intervals from the linear model
#'
#' @param lin_object gam model produced from the mgcv R package with a linear trend fitted
#' @param lin_term name of the linear term
#' @param percent Should this be calculated as a percentage (TRUE/FALSE)
#'
#' @importFrom parameters model_parameters
#'
#' @export

extract_trend <- function(lin_object, lin_term = "trendY", percent = FALSE){
  
  
  message("Using the Bootstrap to compute 95% CIs. This will take a few minutes ...\n")
  if(percent){
    lin_par <- data.frame(model_parameters(lin_object, exponentiate = TRUE, digits = 4, bootstrap = TRUE))
    est <- lin_par[lin_par$Parameter == "trendY", c("Coefficient", "CI_low", "CI_high")]
    names(est) <- c("Est", "CI_high", "CI_low")
    est <- est[,c("CI_low", "Est", "CI_high")]
    est <- round((1-est)*100, 1)
  }
  else{
    lin_par <- data.frame(model_parameters(lin_object, exponentiate = FALSE, digits = 4, bootstrap = TRUE))
    est <- lin_par[lin_par$Parameter == "trendY", c("Coefficient", "CI_low", "CI_high")]
    names(est) <- c("Est", "CI_high", "CI_low")
    est <- est[,c("CI_low", "Est", "CI_high")]
    
  }
  
  
  est
  
  
}