#' @name summary.regdata
#'
#' @title Summarise regularised data
#'
#' @description summarises an object of class "regdata"
#'
#' @param object regdata object
#' @param Qcutoff Qcutoff
#' @param ... other parameters passed to \code{summary}
#'
#' @export

summary.regdata <- function(object, Qcutoff = NULL, ...){

  if(class(object)[1] != "regdata")
    stop("Object is not of class 'regdata'.\n")


    Y <- "WY"
    yr <- unique(object$Qreg$Y)
    n <- table(object$CQ$Y)
    Q1bar <- with(object$Qreg, tapply(pQ, Y, function(x) mean(x, na.rm = TRUE)))
    y <- with(object$Q, CreateY(Date, Y))
    Qbar <- tapply(object$Q$Flow, y, function(x) mean(x, na.rm = TRUE))
    id <- match(names(Q1bar), names(Qbar))
    Qbar <- Qbar[id]

    qbar <- with(object$CQ, tapply(pQ, Y, function(x) mean(x, na.rm = TRUE)))

    sumtab <- data.frame(Year = yr, n = as.vector(n), AvgFlowC = qbar, AvgFlowR = Q1bar,
                       AvgFlowO = Qbar, FlowC.bias = qbar/Q1bar, FlowQ.bias = Q1bar/Qbar)
    row.names(sumtab) <- NULL


    # Data Quality Assessment
    ## no. of samples in the upper 2% of the flow range
    if(is.null(Qcutoff)){
      flow.up2pc <- with(object$Q, quantile(Flow, 0.98, na.rm = TRUE))
    }
    else{
      flow.up2pc <- Qcutoff
    }
    nc2pc <- length(object$CQ$Conc[object$CQ$pQ >= flow.up2pc])
    ## tabulation of samples
    qval <- character(nrow(object$CQ))
    qval <- ifelse(with(object$CQ, pQ <= quantile(object$Q$Flow, 0.25, na.rm = TRUE)), "<25%ile", qval)
    qval <- ifelse(with(object$CQ, pQ <= quantile(object$Q$Flow, 0.5, na.rm = TRUE) & pQ > quantile(object$Q$Flow, 0.25, na.rm = TRUE)), "25%ile-50%ile", qval)
    qval <- ifelse(with(object$CQ, pQ <= quantile(object$Q$Flow, 0.75, na.rm = TRUE) & pQ > quantile(object$Q$Flow, 0.5, na.rm = TRUE)), "50%ile-75%ile", qval)
    qval <- ifelse(with(object$CQ, pQ <= quantile(object$Q$Flow, 0.90, na.rm = TRUE) & pQ > quantile(object$Q$Flow, 0.75, na.rm = TRUE)), "75%ile-90%ile", qval)
    qval <- ifelse(with(object$CQ, pQ <= quantile(object$Q$Flow, 0.95, na.rm = TRUE) & pQ > quantile(object$Q$Flow, 0.9, na.rm = TRUE)), "90%ile-95%ile", qval)
    qval <- ifelse(with(object$CQ, pQ <= quantile(object$Q$Flow, 0.98, na.rm = TRUE) & pQ > quantile(object$Q$Flow, 0.95, na.rm = TRUE)), "95%ile-98%ile", qval)
    qval <- ifelse(with(object$CQ, pQ <= quantile(object$Q$Flow, 0.99, na.rm = TRUE) & pQ > quantile(object$Q$Flow, 0.98, na.rm = TRUE)), "98%ile-99%ile", qval)
    qval <- ifelse(with(object$CQ, pQ <= quantile(object$Q$Flow, 1, na.rm = TRUE) & pQ > quantile(object$Q$Flow, 0.99, na.rm = TRUE)), ">99%ile", qval)
    samp.tab <- tapply(object$CQ$Conc, qval, length)
    percsamp.tab <- numeric(8)
    names(percsamp.tab) <- c("<25%ile", "25%ile-50%ile", "50%ile-75%ile", "75%ile-90%ile",
                             "90%ile-95%ile", "95%ile-98%ile", "98%ile-99%ile", ">99%ile")
    percsamp.tab[names(samp.tab)] <- round(samp.tab/sum(samp.tab) * 100, 2)

    regsum <- list(sumtab = sumtab, percsamp.tab = percsamp.tab, nc2pc = nc2pc, CQ = object$CQ)

    class(regsum) <- "sumregdata"

    regsum
}


