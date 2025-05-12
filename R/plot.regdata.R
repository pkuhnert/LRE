#' @name plot.regdata
#'
#' @title Plot regularised data
#'
#' @description plots an object of class "regdata"
#'
#' @param x x
#' @param Type Year type: water year (WY) or financial year (FY)
#' @param Qcutoff Qcutoff
#' @param ... other parameters passed to \code{plot}
#'
#' @import "ggplot2"
#' @import "gridExtra"
#' @importFrom "reshape2" "melt"
#'
#' @export

plot.regdata <- function(x, Type, Qcutoff = NULL, ...){

  if(!inherits(x, "regdata"))
    stop("Object is not of class 'regdata'.\n")

  if(missing(Type))
    stop("Type not specified. One of 'WY' or 'FY' needs to be specified.\n")


   xlabel <- unique(x$CQ$Y)
   if(length(xlabel) > 1)
      xlabel <- paste(xlabel[1], "to", xlabel[length(xlabel)])

   ###############################################################################################
   # PLOT 1: Rising/Falling Limb
   ###############################################################################################
   un.yr <- names(table(x$CQ$Y))
   un.yr <- un.yr[table(x$CQ$Y) !=0]

   p <- list()
   for(i in 1:length(un.yr)){

        mat <- x$CQ[x$CQ$Y == un.yr[i],]
       yr1 <- substring(un.yr[i], 1, 4)
       if(Type == "WY"){
          start <- paste(yr1, "-10-01 0:00", sep = "")
          stop <- paste(as.numeric(yr1)+1, "-09-30 0:00", sep = "")
       }
       else if(Type == "FY"){
          start <- paste(yr1, "-07-01 0:00", sep = "")
          stop <- paste(as.numeric(yr1)+1, "-06-30 0:00", sep = "")
       }
       else
         stop("Type is not 'WY' or 'FY'. Please specify one and rerun.\n")

       start <- as.POSIXct(start, format = "%Y-%m-%d %H:%M", tz = "GMT")
       stop <- as.POSIXct(stop, format = "%Y-%m-%d %H:%M", tz = "GMT")
       p[[i]] <- ggplot(aes(x = .data[["Date"]], y = .data[["pQ"]]), data = mat) + 
         geom_line() + ylab("Concentration (log scale)") +
         geom_point(aes(x = .data[["Date"]], y = .data[["pQ"]], shape = .data[["limb"]]), 
                    size = 2) + ggtitle(paste("Year: ", un.yr[i], sep = ""))
   }

      if(length(un.yr) == 1)
         pF <- p[[1]]
      else if(length(un.yr) == 2)
        pF <- marrangeGrob(p, ncol = 1, nrow = 2, top = "Rising/Falling Limb")
      else
        pF <- marrangeGrob(p, ncol = 2, nrow = 2, top = "Rising/Falling Limb")




   ###############################################################################################
   # PLOT 2: Distributional summary
   ###############################################################################################
   # Data Quality Assessment
   ## no. of samples in the upper 2% of the flow range
   if(is.null(Qcutoff)){
     flow.up2pc <- with(x$Q, quantile(Flow, 0.98, na.rm = TRUE))
   }
   else{
     flow.up2pc <- Qcutoff
   }

   nc2pc <- length(x$CQ$Conc[x$CQ$pQ >= flow.up2pc])
   ## tabulation of samples
   qval <- character(nrow(x$CQ))
   qval <- ifelse(with(x$CQ, pQ <= quantile(x$Q$Flow, 0.25, na.rm = TRUE)), "<25%ile", qval)
   qval <- ifelse(with(x$CQ, pQ <= quantile(x$Q$Flow, 0.5, na.rm = TRUE) & pQ > quantile(x$Q$Flow, 0.25, na.rm = TRUE)), "25%ile-50%ile", qval)
   qval <- ifelse(with(x$CQ, pQ <= quantile(x$Q$Flow, 0.75, na.rm = TRUE) & pQ > quantile(x$Q$Flow, 0.5, na.rm = TRUE)), "50%ile-75%ile", qval)
   qval <- ifelse(with(x$CQ, pQ <= quantile(x$Q$Flow, 0.90, na.rm = TRUE) & pQ > quantile(x$Q$Flow, 0.75, na.rm = TRUE)), "75%ile-90%ile", qval)
   qval <- ifelse(with(x$CQ, pQ <= quantile(x$Q$Flow, 0.95, na.rm = TRUE) & pQ > quantile(x$Q$Flow, 0.9, na.rm = TRUE)), "90%ile-95%ile", qval)
   qval <- ifelse(with(x$CQ, pQ <= quantile(x$Q$Flow, 0.98, na.rm = TRUE) & pQ > quantile(x$Q$Flow, 0.95, na.rm = TRUE)), "95%ile-98%ile", qval)
   qval <- ifelse(with(x$CQ, pQ <= quantile(x$Q$Flow, 0.99, na.rm = TRUE) & pQ > quantile(x$Q$Flow, 0.98, na.rm = TRUE)), "98%ile-99%ile", qval)
   qval <- ifelse(with(x$CQ, pQ <= quantile(x$Q$Flow, 1, na.rm = TRUE) & pQ > quantile(x$Q$Flow, 0.99, na.rm = TRUE)), ">99%ile", qval)
   samp.tab <- tapply(x$CQ$Conc, qval, length)
   percsamp.tab <- numeric(8)
   names(percsamp.tab) <- c("<25%ile", "25%ile-50%ile", "50%ile-75%ile", "75%ile-90%ile",
                            "90%ile-95%ile", "95%ile-98%ile", "98%ile-99%ile", ">99%ile")
   percsamp.tab[names(samp.tab)] <- round(samp.tab/sum(samp.tab) * 100, 2)

   percsamp.df <- data.frame(y = percsamp.tab, x = names(percsamp.tab))
   row.names(percsamp.df) <- NULL
   pD <- ggplot(aes(x = .data[["x"]], y = .data[["y"]]), data = percsamp.df) + 
     geom_bar(stat = "identity") + coord_flip() +
     xlab("Percentage of samples captured") + ylab("") + 
     ggtitle("Percentage of samples captured by Flow")

   ###############################################################################################
   # PLOT 3: plot of concentration versus discharge
   ###############################################################################################
   mat.nona <- x$CQ[x$CQ$Conc != 0,]
   pS1 <- ggplot(aes(x = .data[["pQ"]], y = .data[["Conc"]]), data = mat.nona) + 
     geom_point() + xlab("Discharge (m3/s)") + ylab("Concentration (mg/L)") +
     geom_vline(xintercept = quantile(x$Q$Flow, c(0.95, 0.98, 0.99), na.rm = TRUE), colour = c("green", "blue", "black"))
   mat.nona$lpQ <- log(mat.nona$pQ)
   mat.nona$lConc <- log(mat.nona$Conc)
   pS2 <- ggplot(aes(x = .data[["lpQ"]], y = .data[["lConc"]]), data = mat.nona) + 
     geom_point() + xlab("log(Discharge)") +
     ylab("log(Concentration)") +
     geom_vline(xintercept = log(quantile(x$Q$Flow, c(0.95, 0.98, 0.99), na.rm = TRUE)), 
                colour = c("green", "blue", "black")) +
     geom_smooth(method = "loess", size = 1.5, col = "red")

   pS <- marrangeGrob(list(pS1, pS2), ncol = 1, nrow = 2, top = "Summaries of concentration and flow")


   ###############################################################################################
   # PLOT 4: plot Smoothing parameters
   ###############################################################################################

   QregM <- melt(data = x$Qreg, id.vars = 1, measure.vars = c("pQ", "MA1day", "MA2days",
                                                              "MAweek", "MAmonth", "MA3months",
                                                              "MA6months", "MA12months"))
   pSm <- ggplot(data = QregM, aes(x = .data[["Date"]], y = .data[["value"]], 
                                   color = 'variable')) + geom_line(linewidth = 1) + 
     ggtitle("Smoothing Parameters") + xlab("") + ylab("Flow (m3/s)")

     list(p_RiseFallLimb = pF, p_DistSum = pD, p_CQsum = pS, p_SmoothParms = pSm)




}

