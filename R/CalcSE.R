#' @rdname LRE-predict-Internal
#' @useDynLib LRE, .registration = TRUE
CalcSE <-function(Xdesign, LhatC, alpha1, alpha2, beta1, rho, W, sig2){

    if(!is.loaded("calcSE")) dyn.load("calcSE.dll")

    len <- ncol(Xdesign)
    n <- nrow(Xdesign)
    storage.mode(Xdesign) <- storage.mode(LhatC) <- storage.mode(alpha1) <- "double"
    storage.mode(alpha2) <- storage.mode(beta1) <- storage.mode(rho) <- "double"
    storage.mode(W) <- storage.mode(sig2) <- "double"
    storage.mode(n) <- storage.mode(len) <- "integer"

   .C("calcSE", Xdesign, LhatC, alpha1 = alpha1, alpha2 = alpha2, beta1 = beta1,
                rho, W, sig2, len, n, SEload = double(1))$SEload

}

