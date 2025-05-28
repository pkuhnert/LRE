#' @name LRE-diagnostic-Internal
#'
#' @title Functions to examine diagnostics of fitted model
#'
#' @description Produces a ggplot version of a Q-Q plot for the residuals
#'
#'
#' @param x residuals
#' @param distribution distribution (Default: "norm")
#' @param line.estimate line estimate (Default: NULL)
#' @param conf confidence limits (Default: 0.95)
#' @param labels labels (Default: names(x))
#'
#' @details Taken from GitHub rentrop/gg_qq.r (Parts of code copied from car:::qqPlot)
#'
#' @import lubridate
#' @importFrom stats ppoints
#' @importFrom stats quantile
#' @importFrom rlang .data
#' 
#' @aliases gg_qq
#' @keywords internal
NULL

#' @keywords internal
gg_qq <- function(x, distribution = "norm", line.estimate = NULL, conf = 0.95,
                  labels = names(x), ...){

  q.function <- eval(parse(text = paste0("q", distribution)))
  d.function <- eval(parse(text = paste0("d", distribution)))
  x <- na.omit(x)
  ord <- order(x)
  n <- length(x)
  P <- ppoints(length(x))
  df <- data.frame(ord.x = x[ord], z = q.function(P, ...))

  if(is.null(line.estimate)){
    Q.x <- quantile(df$ord.x, c(0.25, 0.75))
    Q.z <- q.function(c(0.25, 0.75), ...)
    b <- diff(Q.x)/diff(Q.z)
    coef <- c(Q.x[1] - b * Q.z[1], b)
  } else {
    coef <- coef(line.estimate(ord.x ~ z))
  }

  zz <- qnorm(1 - (1 - conf)/2)
  SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n)
  fit.value <- coef[1] + coef[2] * df$z
  df$upper <- fit.value + zz * SE
  df$lower <- fit.value - zz * SE

  if(!is.null(labels)){
    df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, labels[ord],"")
  }

  p <- ggplot(df, aes(x = .data[["z"]], y = .data[["ord.x"]])) +
    geom_point() +
    geom_abline(intercept = coef[1], slope = coef[2]) +
    geom_ribbon(aes(ymin = .data[["lower"]], ymax = .data[["upper"]]), alpha = 0.2)
  if(!is.null(labels)) p <- p + geom_text( aes(label = labels))
  p
}
