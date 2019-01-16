#' @rdname LRE-predict-Internal
BlockSigma <- function(len, Xdesign, W){

 n <- nrow(Xdesign)
 nB <- trunc(n/len)
 Sigma <- list()
 for(i in 1:nB){
     start <- (i-1)*len + 1
     stop <- i*len
     Sigma[[i]] <- diag(Xdesign[start:stop,] %*% W %*%
                     t(Xdesign[start:stop,]))
 }
 start <- nB*len+1
 stop <- n
 Sigma[[nB+1]] <- diag(Xdesign[start:stop,] %*% W %*%
                     t(Xdesign[start:stop,]))

 Sigma

}

