#' CreateLD
#' 
#' LRE Internal function
#' 
#' @param Q Flow data
#' @param Conc Concentration data
#' @param date.range Date range
#' @param samp.unit Sampling unit
#' @param Ytype Year type. One of water year ("WY" or "WY2") or financial year ("FY")
#' @param Qflush Quantile for "first flush"
#' @param Reg Parameters for regularisation (type, rainfall, date)
#'
CreateLD <- function(Q, Conc, date.range, samp.unit, Ytype,
                    Qflush, Reg = list(type = "none", rainfall = NULL, date = NULL)){


   #---------------- Preliminaries --------------------------------------#
   # Remove any missing records in Q or Conc
   Q <- na.omit(Q)
   if(ncol(Conc) != 2) stop("Concentration dataset should have 2 columns: Date, Concentration\n")
   names(Conc) <- c("Date", "Conc")
   Conc <- na.omit(Conc)
   # replace 0 records of flow with 0.01, a small value
   # avoids log(0)
   Q$Flow[Q$Flow == 0] <- 0.01
   # Create a Y variable: for estimating loads each year (WY/FY)
   Q$Y <- CreateY(Q$Date, Ytype = Ytype)

   #-----------------------  Extract modelling dataset ----------------------#
 
   if(!is.null(date.range$model)){
      # Restrict concentration data to specific dates set by user
      dateR <- as.Date(as.POSIXct(date.range$model, format = "%d-%m-%Y %H:%M:%S", tz = "GMT"),
         format = "%d-%m-%Y %H:%M:%S", tz = "GMT")
      mind <- dateR[1]
      maxd <- dateR[2]
   }
   else{
      # Restrict flow data
      mind <- as.Date(min(Conc$Date))
      maxd <- as.Date(max(Conc$Date))
   }
   indC <- with(Conc, as.Date(Date) >= mind & as.Date(Date) <= maxd)
   Conc <- Conc[indC,]
 
   #----------------------------- Extract prediction dataset --------------------------------#
   if(!is.null(date.range$pred)){
      # Restrict flow data to specific dates set by user
      dateR <- as.Date(as.POSIXct(date.range$pred, format = "%d-%m-%Y", tz = "GMT"), format = "%d-%m-%Y", tz = "GMT")
      mind <- dateR[1]
      maxd <- dateR[2]
   }
   else{
      # Restrict flow to concentration data
      mind <- as.Date(min(Conc$Date))
      maxd <- as.Date(max(Conc$Date))
   }
   ind <- as.Date(Q$Date) >= mind & as.Date(Q$Date) <= maxd
   Q <- Q[ind,]


   #-------------------------------- REGULARISATION --------------------------------------------#
 
   Qreg <- CreateQregDataset(Q = Q, samp.unit = samp.unit, Qflush = Qflush, Ytype = Ytype,
                            Reg = Reg)


   #----------------------------- Creating modelling dataset by matching ------------------------#
   #----------------------------- concentration dates with flow dates ---------------------------#
 
   val <- CreateCQDataset(Conc = Conc, Qreg = Qreg, startdate = min(Q$Date))

   CQ <- val$CQ
   Cm <- val$Cm



   PlotCQ(Cm, Qreg)


   list(Cm = CQ, Qreg = Qreg)
}

