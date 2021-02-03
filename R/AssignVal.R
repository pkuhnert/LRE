#' AssignVal
#' 
#' LRE function used internall
#' 
#' @param x vector of x-values
#' @param y vector of y-values
#'
AssignVal <- function(x, y){


  id <- sapply(as.numeric(x), function(x, y){
                            val <- abs(x-y)
                            (1:length(val))[val == min(val)][1]}  , as.numeric(y))

  id
}

