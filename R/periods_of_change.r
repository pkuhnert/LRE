#' @name periods_of_change
#'
#' @title Calculate and Plot Periods of Change in a non-linear curve
#'
#' @description Calculates and plots the episodic trends in a non-linear trend
#'        curve produced from a gam
#'
#' @param object gam model produced from the mgcv R package
#' @param Term term to calculate the periods of change over, typically a trend term
#' @param data data used in the gam model, eg loaddata$CQ
#' @param object.vars variables used in the gam fit.
#' @param x_label label for the x-axis.  If NULL, it will default to numbers starting at 1.
#' @param spacing white space left between the labels. Defaults to 5.
#' @param nsamps  Typically NULL but can be a vector of the number of samples shown for each point in time.
#' @param alpha significance level. Default is 0.05.
#'
#' @import ggplot2
#' @import patchwork
#' @importFrom stats qt df.residual confint acf ts
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#' @export

periods_of_change <- function(object, Term = "trendY", data, 
                              object.vars = c("pQ", "MA2days", "MAweek", "MAmonth", 
                                           "MA6months", "MA12months", "trendY"), x_label = NULL,
                              spacing = 5, nsamps = NULL, alpha = 0.05){
  

  n <- length(x_label)*spacing
  d <- Deriv(object, data = data, object.vars = object.vars, n = n)
  object.d <- d$lD
  newDF <- d$newDF
  
  p2 <- predict.gam(object, newdata = newDF, type = "terms", se.fit = TRUE)
  
 
  pdat <- data.frame(years = newDF[,Term],  p2 = unique(p2$fit[,paste0("s(", Term, ")")]), 
                     se2 = unique(p2$se.fit[,paste0("s(", Term, ")")]))
  

  df.res <- df.residual(object)
  crit.t <- qt(alpha/2, df.res, lower.tail = FALSE)
  pdat <- mutate(pdat,
                    upper = .data[["p2"]] + (crit.t * .data[["se2"]]),
                    lower = .data[["p2"]] - (crit.t * .data[["se2"]]))
  
  object.dci <- confint(object.d, term = Term)
  object.dsig <- signifD(pdat$p2, d = object.d[[Term]]$deriv,
                       object.dci[[Term]]$upper, object.dci[[Term]]$lower)
  
  pdat$incr <- object.dsig$incr
  pdat$decr <- object.dsig$decr

  if(is.null(nsamps)){
    fullrange <- 1:max(data[,Term])
    tab <- table(factor(data[,Term], levels = fullrange))
    
  }
  else{
    tab <- nsamps
    
  }
  
  proportion <- as.numeric(tab)/max(table(data[,Term]))
  prop_df  <- data.frame(proportion = proportion, year_index = 1:max(data[,Term]))
  

  pdat$prop <- rep(NA, n)
  
  # Apply it
  out_length <- nrow(prop_df) * spacing
  padded <- rep(NA, out_length)
  padded[seq(1, out_length, by = 5)] <- prop_df$proportion
  pdat$prop <- padded
  
  
    bar_plot <- ggplot(pdat[!is.na(pdat$prop), ], aes(x = .data[["years"]], y = .data[["prop"]])) +
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
  
 
  trend_plot <- ggplot(pdat, aes(x = .data[["years"]], y = .data[["p2"]])) +
    geom_ribbon(aes(ymin = .data[["lower"]], ymax = .data[["upper"]]), fill = "grey70", alpha = 0.5) +  # Confidence interval
    geom_line(color = "black") +  # Main curve
    geom_line(aes(y = .data[["incr"]]), color = "red", size = 1, na.rm = TRUE) +  # Increasing segments
    geom_line(aes(y = .data[["decr"]]), color = "blue", size = 1, na.rm = TRUE) +  # Decreasing segments
    theme_minimal() +
    xlab("") + ylab("Trend Contribution" ) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size = 8),
      plot.margin = margin(0, 10, 0, 0)  # tight spacing above/below
    )

  trend_plot / bar_plot + plot_layout(heights = c(1, 0.25))
  
}

