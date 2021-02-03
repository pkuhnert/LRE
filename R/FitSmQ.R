#' FitSmQ
#' 
#' LRE Internal function.
#' 
#' @param Q flow
#' @param Qreg regularised flow
#' @param method spline method for interpolation
#'
#'
#' @importFrom "stats" "splinefun"
FitSmQ <- function(Q, Qreg, method = "monoH.FC"){


 # Using splinefun
 f <- splinefun(Q$days, log(Q$Flow), method = method)
 Qreg$pQ <- exp(f(Qreg$days))

 Q$lFlow <- log(Q$Flow)
 Qreg$lpQ <- log(Qreg$pQ)
 pA1 <- ggplot(aes_string('days', 'lFlow'), data = Q) + geom_line()
 pA2 <- pA1 + xlab("Days") + ylab("log(Flow)") + ggtitle("log-scale")
 pA3 <- pA2 + geom_point(aes_string('days', 'lpQ'), data = Qreg, colour = "purple")

 pB1 <- ggplot(aes_string('days', 'Flow'), data = Q) + geom_line()
 pB2 <- pB1 + xlab("Days") + ylab("Flow (CM/s)") + ggtitle("natural scale")
 pB3 <- pB2 + geom_point(aes_string('days', 'pQ'), data = Qreg, colour = "purple")

 p <- marrangeGrob(list(pA3, pB3), ncol = 1, nrow = 2, top = "Regularisation")
 print(p)

 list(smf = f, Qreg = Qreg)

}

