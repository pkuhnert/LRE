#' @importFrom "stats" "qnorm"
#' @rdname LRE-predict-Internal
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
