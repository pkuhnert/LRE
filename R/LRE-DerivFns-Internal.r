#' @name LRE-DerivFns-Internal
#'
#' @title Internal functions for \code{periods_of_change} function.
#'
#' @description Internal functions for calculating and plotting trends on a 
#'              non-linear curve. Reference for this concept appears in the link below by Gavin Simpson:
#               https://fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
#               with modifications by Petra Kuhnert
#'
#' @param object gam model object
#' @param n number of iterations to calculate derivatives. Default is 200.
#' @param eps A small positive constant representing the convergence threshold. 
#' @param data data used in the gam model.
#' @param object.vars variables used in the gam fit.
#' @param plot.Deriv plot.Deriv function
#' @param term varible in the model object that is of interest.  Typically trend term.
#' @param alpha significance level. Default is 0.05.
#' @param ... additional arguments to base plot function
#' @param x predicted value from model
#' @param d calculated derivative
#' @param upper upper confidence interval
#' @param lower lower confidence interval
#' @param eval evaluation point.  Defaults to 0.
#' 
#' @aliases Deriv confint.Deriv signifD
#' @keywords internal
#'
#'
#' @importFrom rlang .data
#'
NULL

#' @keywords internal
  Deriv <- function(object, n = 200, eps = 1e-7, data, object.vars) {  
  
  if(any(object.vars == "limb")){
    newD <- data.frame(sapply(data.frame(as.matrix(data[,object.vars])),
                              function(x) seq(min(x), max(x), length = n)))
    newD[,"limb"] <- rep(c(-1, 0, 1), length.out = n)
  }
  else{
    newD <- data.frame(sapply(data.frame(as.matrix(data[,object.vars])),
                              function(x) seq(min(x), max(x), length = n)))
  }
  newDF <- data.frame(newD) ## needs to be a data frame for predict
  names(newDF) <- object.vars
  X0 <- predict.gam(object, newDF, type = "lpmatrix")
  if(any(object.vars == "limb")){
    newDF <- newDF + eps
    newDF$limb <- newD$limb
  }
  else
    newDF <- newDF + eps
  
  X1 <- predict.gam(object, newDF, type = "lpmatrix")
  Xp <- (X1 - X0) / eps
  Xp.r <- NROW(Xp)
  Xp.c <- NCOL(Xp)
  ## dims of bs
  bs.dims <- sapply(object$smooth, "[[", "bs.dim") - 1
  ## number of smooth terms
  t.labs <- attr(object$terms, "term.labels")
  
  nt <- length(t.labs)
  ## list to hold the derivatives
  lD <- vector(mode = "list", length = nt)
  names(lD) <- t.labs
  for(i in seq_len(nt)) {
    Xi <- Xp * 0
    want <- grep(t.labs[i], colnames(X1))
    Xi[, want] <- Xp[, want]
    df <- Xi %*% coef(object)
    df.sd <- rowSums(Xi %*% object$Vp * Xi)^.5
    lD[[i]] <- list(deriv = df, se.deriv = df.sd)
  }
  
  
  lD$gamModel <- object
  lD$eps <- eps
  lD$eval <- newD - eps
  
  list(lD = lD, newDF = newDF)
}

# Calculating confidence intervals
confint <- function(object, term, alpha = 0.05, ...) {
  l <- length(object) - 3
  term.labs <- names(object[seq_len(l)])
  if(missing(term)) {
    term <- term.labs
  } else { 
    term <- term.labs[match(term, term.labs)]
  }
  if(any(miss <- is.na(term)))
    stop(paste("'term'", term[miss], "not a valid model term."))
  res <- vector(mode = "list", length = length(term))
  names(res) <- term
  residual.df <- df.residual(object$gamModel)
  tVal <- qt(1 - (alpha/2), residual.df)
  
  for(i in term) {
    upr <- object[[i]]$deriv + tVal * object[[i]]$se.deriv
    lwr <- object[[i]]$deriv - tVal * object[[i]]$se.deriv
    res[[i]] <- list(upper = drop(upr), lower = drop(lwr))
  }
  res$alpha = alpha
  res
}

# Establishing significance
signifD <- function(x, d, upper, lower, eval = 0) {
  miss <- upper > eval & lower < eval
  incr <- decr <- x
  want <- d > eval
  incr[!want | miss] <- NA
  want <- d < eval
  decr[!want | miss] <- NA
  list(incr = incr, decr = decr)
}

