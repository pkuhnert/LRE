#' @importFrom "stats" "coef"
#' @rdname LRE-predict-Internal
EstLoadYS <- function(CQdat, Qdat, concY, modobj, samp.unit = samp.unit,
  alpha1, alpha2, se = TRUE, Blocklen=5000){

  # samp.unit = sampling unit the predicted flow is at

  uY <- unique(CQdat$CQhat$Y)
  loadest <- matrix(NA, nrow = length(uY), ncol = 6)
  loadest <- data.frame(loadest)
  names(loadest) <-  c("Y", "Total_Flow_ML", "TLhat_Mt", "TLhatC_Mt", "SEload", "CV")

  loadest.d <- NULL
  for(i in 1:length(uY)){
    cat("Year: ", uY[i], "\n")
    CQhatY <- CQdat$CQhat[CQdat$CQhat$Y == uY[i],]
     

    QdatY <- Qdat[Qdat$Y == uY[i],]

    Xdesign <- as.matrix(CQdat$Xdesign[Qdat$Y == uY[i],])

    if(length(modobj) == 2){
       coef1 <- coef(modobj$gam)[pmatch("LQflow", names(coef(modobj)))]
       if(length(coef1) == 1)
          coef2 <- NA
       else{
          coef2 <- coef1[2]
          coef1 <- coef1[1]
       }
    }
    else
      {
       coef1 <- coef(modobj)[pmatch("LQflow", names(coef(modobj)))]
       if(length(coef1) == 1)
          coef2 <- NA
       else{
          coef2 <- coef1[2]
          coef1 <- coef1[1]
       }
    }

    if(is.na(coef1) & is.na(coef2))
       beta1 <- 0  # no flow terms fitted
    else if(is.na(coef2) & !is.na(coef1))
       beta1 <- coef1
    else
       beta1 <- coef1 + 2 * coef2 * log(QdatY$pQ)


    est <- EstLoad(CQdat = CQhatY, Qdat = QdatY, Xdesign = Xdesign,
                       W = CQdat$W, sig2 = CQdat$sig2, rho = CQdat$rho, alpha1 = alpha1,
                       alpha2 = alpha2, beta1 = beta1,
                       samp.unit = samp.unit, se = TRUE)

    loadest[i,] <- est$est.annual
    loadest.d <- rbind(loadest.d, est$est.daily)

  }

  names(loadest.d) <- c("Y", "Date", "DLhat_Mt", "DLhatC_Mt", "D_Flow_ML", "SEload", "CV")
  # Add Y
  loadest$Y <- uY
  # Add number of recorded concentration samples

  tabY <- table(concY)
  ind <- match(names(tabY), as.vector(loadest$Y))
  loadest$n <- 0
  loadest$n[ind[!is.na(ind)]] <- tabY[!is.na(ind)]
  list(loadest.annual = loadest, loadest.daily = loadest.d)
}

