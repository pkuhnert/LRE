#' @title FitModel
#'
#' @description This function fits a generalized additive model
#' to a loads dataset
#'
#' @usage FitModel(x, parms = list(flow = FALSE, seasonal = FALSE,
#'           RFlimb = FALSE, MA = c(MA1day = FALSE, MA2days = FALSE,
#'           MAweek = FALSE, MAmonth = FALSE, MA6months = FALSE,
#'           MA12months = FALSE), trend = FALSE, correlation = FALSE))
#'
#' @param x The loads dataset, CQ object resulting from the \code{\link{CreateData}}
#' function.
#' @param parms A list comprising terms to fit in a model:
#'   flow: linear (\code{"linear"}) and quadratic (\code{"quadratic"}) terms for flow.
#'   Default: \code{FALSE}
#'   seasonal: logical term indicating whether seasonal terms are fitted. Default:\code{FALSE}
#'   RFlimb: logical term indicating whether a rising/falling limb term is fitted.
#'   Default: \code{FALSE},
#'   MA: vector indicating whether one or more moving average terms are fitted.
#'   Default:\code{c(MA1day = FALSE, MA2days = FALSE, MAweek = FALSE, MAmonth = FALSE,
#'   MA6months = FALSE, MA12months = FALSE)}
#'   trend: logical term indicating whether a long term trend is fitted. Default: \code{FALSE}
#'   correlation: AR1 correlation structure incorporated. Default: \code{FALSE}
#'
#' @details This is a wrapper function for fitting a generalised additive model using the
#' \code{link{mgcv}} package. Terms are fitted in the model by "switching" terms on and off.
#' Apart from the \code{flow} term, all other terms are logical. If a term is intended to be
#' included in a model, then that term can be set to \code{TRUE}.
#'
#' To include additional terms to those specified, the user will need to run the \code{gam}
#' function from the \code{\link{mgcv}} package that this method is based upon. The following is
#' an example where an additional variable, "dam" is included into the model.
#' \code{mod <- gam(log(Conc) ~ log(pQ) + I(log(pQ)^2) + per(days, k=2) +  s(MA1day) + s(trend) + dam,
#'       family = gaussian, data = loaddata$CQ, control = gam.control(keepData = TRUE)) }
#'       where \code{loaddata$CQ} contains an additional column of data labelled, "dam".
#'
#' @return an object of class \code{fitmodel} with elements as described in
#' \code{gamObject} from the \code{mgcv} package. Standard \code{summary}
#' and \code{anova} functions apply.
#'
#' @references Kuhnert, P.M., Henderson, B.L., Lewis, S.E.,
#' Bainbridge, Z.T., Wilkinson, S.N. and Brodie, J.E. (2012)
#' Quantifying total suspended sediment export from the Burdekin
#' River catchment using the loads regression estimator tool,
#' Water Resources Research, 48, W04533,doi:10.1029/2011WR011080.
#'
#' @importFrom "methods" "Quote"
#' @import "mgcv"
#'
#' @seealso ReadInData, CreateData, gam, diagnostic
#'
#' @keywords gam, loads
#'
#' @export



FitModel <- function(x, parms = list(flow = FALSE, seasonal = FALSE,
     RFlimb = FALSE, MA = c(MA1day = FALSE, MA2days = FALSE, MAweek = FALSE, MAmonth = FALSE, MA6months = FALSE, MA12months = FALSE),
     trend = FALSE, correlation = FALSE)){

     # Might need to put a clause in here to take into account models with only
     # linear terms. If this happens, use glm and gls to fit the models with and
     # without autocorrelation. This means changing the plot, summary and diagnostic functions

    if(parms$RFlimb == TRUE & length(table(x$limb)) != 3){
       parms$RFlimb <- FALSE
       warning("RFlimb set to FALSE because not all limb categories represented. \n")
    }

    xterms <- mod <- list()
    xknots <- NULL

    ######################################################################################
    # Flow terms
    ######################################################################################
    if(parms$flow == "quadratic")
       xterms$flow <- Quote(LQflow(pQ, quad = TRUE))
    else if(parms$flow == "linear")
       xterms$flow <- Quote(LQflow(pQ, quad = FALSE))
    else{
       warning("Model specified is not listed. Defaulting to linear term for flow only.\n")
       xterms$flow <- Quote(LQflow(pQ, quad = FALSE))
    }
    ######################################################################################
    # Seasonal term
    ######################################################################################

    # seasonal
    if(parms$seasonal){
       xterms$seasonal <- Quote(s(month, bs = "cc"))
       xknots <- Quote(list(month = seq(1, 13, length = 10)))
    }

    ######################################################################################
    # Rising falling limb term
    ######################################################################################

    # Rising/Falling Limb
    if(parms$RFlimb)
       xterms$limb <- Quote(limb)

    ######################################################################################
    # MA terms
    ######################################################################################

    xterms$MA <- vector(mode = "list", length = 6)
    names(xterms$MA) <- c("MA1day", "MA2days", "MAweek", "MAmonth", "MA6months", "MA12months")

    # MA terms
    if(parms$MA["MA1day"])
      xterms$MA$MA1day <- Quote(s(MA1day, bs = "cr"))

    if(parms$MA["MA2days"])
      xterms$MA$MA2days <- Quote(s(MA2days, bs = "cr"))


    if(parms$MA["MAweek"])
      xterms$MA$MAweek <- Quote(s(MAweek, bs = "cr"))

    if(parms$MA["MAmonth"])
      xterms$MA$MAmonth <- Quote(s(MAmonth, bs = "cr"))

    if(parms$MA["MA6months"])
      xterms$MA$MA6months <- Quote(s(MA6months, bs = "cr"))

    if(parms$MA["MA12months"])
      xterms$MA$MA12months <- Quote(s(MA12months, bs = "cr"))



    ######################################################################################
    # Trend term
    ######################################################################################

    # Trend
    if(parms$trend)
       xterms$trend <- Quote(s(trendY, bs = "cr"))

    ######################################################################################
    # Fitting model (with and without correlation)
    ######################################################################################
    if(parms$correlation){
       cat("Fitting a Generalized Additive Model with an AR1 correlation structure. \nThis can take time for longer time series ...\n")
       fitCommand <- Quote({
           mod <- try(gamm(as.numeric(log(Conc)) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, family = gaussian, data = x,
                     knots = k, correlation = corAR1(form = ~ time.index), keep.data = TRUE), silent = TRUE)
       })
    }
    else{
      cat("Fitting a Generalized Additive Model ...\n")


      fitCommand <- Quote({
        mod <- try(gam(log(Conc) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, family = gaussian, data = x,
                       knots = k, control = gam.control(keepData = TRUE)), silent = TRUE)
      })


   }

    thisFitCommand <- do.call("substitute", list(fitCommand,
                                                 list(x1 = xterms$flow, x2 = xterms$seasonal, x3 = xterms$limb,
                                                      x4 = xterms$MA[[1]], x5 = xterms$MA[[2]], x6 = xterms$MA[[3]],
                                                      x7 = xterms$MA[[4]], x8 = xterms$MA[[5]], x9 = xterms$MA[[6]],
                                                      x10 = xterms$trend, k = xknots)))

    eval(thisFitCommand)
    
    oldClass(mod) <- c("fitmodel", class(mod))
    mod

}

