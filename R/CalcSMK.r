#' @name CalcSMK
#'
#' @title Calculating SMK
#'
#' @description Calculating SMK using regularised data
#'
#' @param loaddata list from LRE that contains the data
#' @param alternative the alternative hypothesis.  This can be changed to test for
#               a particular type of trend e.g. negative -> "less"
#'
#' @importFrom trend smk.test sens.slope
#' @importFrom dplyr %>% group_by summarise arrange ungroup
#' @importFrom stats ts acf
#' @importFrom rlang .data
#'
#' @export
CalcSMK <- function(loaddata, alternative = "two.sided"){
  
  # regularised data
  df <- loaddata$CQ[, c("Date", "Conc", "month", "trendY")]
  names(df)[4] <- "Year"
  
  # Aggregate to monthly median (or mean) concentration
  monthly_df <- df %>%
    group_by(.data[["Year"]], .data[["month"]]) %>%
    summarise(Conc = median(.data[["Conc"]], na.rm = TRUE)) %>%
    arrange(.data[["Year"]], .data[["month"]]) %>%
    ungroup()
  
  # Convert to time series object
  # Find the starting year and month
  start_year <- min(monthly_df$Year)
  start_month <- monthly_df$month[monthly_df$Year == start_year][1]
  
  # Create time series (monthly frequency)
  conc_ts <- ts(monthly_df$Conc, start = c(start_year, start_month), frequency = 12)
  
  # Run the Seasonal Mann-Kendall test
  smk_result <- smk.test(conc_ts, alternative = alternative)
  sens_result <- sens.slope(conc_ts)
  sslope <- sens.slope(conc_ts)
  
  
  # Check for autocorrelation
  # Check autocorrelation using ACF plot
  acf(conc_ts, main = "Autocorrelation of Monthly Concentration")
  
  # Or compute lag-1 autocorrelation
  acf_values <- acf(conc_ts, plot = FALSE)
  lag1_rho <- acf_values$acf[2]
  
  
  list(lag1_rho = lag1_rho, smk = smk_result, slope = sslope)
  
  
  
  
}



