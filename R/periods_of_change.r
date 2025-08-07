#' @name periods_of_change
#'
#' @title Calculate and Plot Periods of Change in a non-linear curve
#'
#' @description Calculates and plots the episodic trends in a non-linear trend
#'        curve produced from a gam
#'
#' @param fit_sm gam object with smoother for trend produced from the mgcv R package
#' @param fit_lin gam object with linear trend term produced from the mgcv R package (Default: NULL)
#' @param Term term to calculate the periods of change over, typically a trend term
#' @param data data used in the gam model, eg loaddata$CQ
#' @param object.vars variables used in the gam fit.
#' @param spacing white space left between the labels. Defaults to 5.
#'
#' @import ggplot2
#' @import patchwork
#' @importFrom stats qt df.residual confint acf ts
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#' @export

periods_of_change <- function(fit_sm, fit_lin = NULL, Term = "trendY", data, 
                              object.vars = c("pQ", "MA2days", "MAweek", "MAmonth", 
                                           "MA6months", "MA12months", "trendY"), 
                              spacing = 5){
  

  # Get labelling
  year <- as.numeric(format(data$Date, format = "%Y"))
  yr_min <- min(year)
  yr_max <- max(year)
  a <- yr_min:(yr_max-1)
  b <- (yr_min+1):yr_max
  x_label <- paste0(a, "/", b) 
  
  # Find Derivatives
  n <- length(x_label)*spacing
  d <- Deriv(fit_sm, data = data, object.vars = object.vars, n = n)
  fit_sm.d <- d$lD
  df_sm <- d$newDF
  
  p_sm <- predict.gam(fit_sm, newdata = df_sm, type = "terms", se.fit = TRUE)
  pdf_sm <- data.frame(years = df_sm$trendY,  p = unique(p_sm$fit[,"s(trendY)"]), 
                       se2 = unique(p_sm$se.fit[,"s(trendY)"]))
  if(!is.null(fit_lin)){
    # If linear term is needed in plotting
    p_lin <- predict.gam(fit_lin, newdata = df_sm, type = "terms",se.fit = TRUE)
    pdf_lin <- data.frame(years = df_sm$trendY, p = unique(p_lin$fit[,"trendY"], type="link"))
    
    # Rescale linear term to match smooth term
    intercept <- coef(fit_lin)["(Intercept)"] 
    pdf_lin$p <- pdf_lin$p + intercept
    pdf_lin$p <- pdf_lin$p-mean(pdf_lin$p)
    
    
    
  }
    
  
  # Computing significant trends on nonlinear curve
  df.res <- df.residual(fit_sm)
  crit.t <- qt(0.025, df.res, lower.tail = FALSE)
  pdf_sm <- transform(pdf_sm,
                      upper = .data$p + (crit.t * .data$se2),
                      lower = .data$p - (crit.t * .data$se2))
  
  fit_sm.dci <- confint(fit_sm.d, term = Term)
  fit_sm.dsig <- signifD(pdf_sm$p, d = fit_sm.d[[Term]]$deriv,
                         fit_sm.dci[[Term]]$upper, fit_sm.dci[[Term]]$lower)
  
  pdf_sm$incr <- fit_sm.dsig$incr
  pdf_sm$decr <- fit_sm.dsig$decr
  
  fullrange <- 1:max(data$trendY)
  tab <- table(factor(data$trendY, levels = fullrange))
  
  proportion <- as.numeric(tab)/max(table(data$trendY))
  prop_df  <- data.frame(proportion = proportion, year_index = 1:max(data$trendY))
  
  pdf_sm$prop <- rep(NA, n)
  # formatting of x-axis labelling
  out_length <- nrow(prop_df) * spacing
  padded <- rep(NA, out_length)
  padded[seq(1, out_length, by = 5)] <- prop_df$proportion
  pdf_sm$prop <- padded
  
  # Plotting
  bar_plot <- ggplot(pdf_sm[!is.na(pdf_sm$prop), ], aes(x = .data$years, y = .data$prop)) +
    geom_col(width = 0.8, fill = "lightblue") +
    scale_x_continuous(breaks = seq(1, length(x_label)), labels = x_label) +
    theme_minimal() +
    ylab("Proportion") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size = 8),
      panel.grid = element_blank(),
      plot.margin = margin(0, 10, 8, 0)
    )
  
  trend_plot <- ggplot(pdf_sm, aes(x = .data$years, y = .data$p)) +
    geom_ribbon(aes(ymin = .data$lower, ymax = .data$upper), fill = "grey70", alpha = 0.5) +  # Confidence interval
    geom_line(color = "black") +  # Main curve
    geom_line(aes(y = .data$incr), color = "red", size = 1, na.rm = TRUE) +  # Increasing segments
    geom_line(aes(y = .data$decr), color = "blue", size = 1, na.rm = TRUE) +  # Decreasing segments
    theme_minimal() +
    xlab("") + ylab("Trend Contribution" ) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size = 8),
      plot.margin = margin(0, 10, 0, 0)  # tight spacing above/below
    )
  if(!is.null(fit_lin)){
    # Overlay linear trend
    trend_plot <- trend_plot + 
      geom_line(data = pdf_lin, aes(x = .data$years, y = .data$p), color = "grey60", lty = "dashed") 
  }
  
  trend_plot / bar_plot + plot_layout(heights = c(1, 0.25))
}

