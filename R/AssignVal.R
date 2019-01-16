#' @describeIn LRE-CreateData-Internal Assign Value
#'
AssignVal <- function(x, y){


  id <- sapply(as.numeric(x), function(x, y){
                            val <- abs(x-y)
                            (1:length(val))[val == min(val)][1]}  , as.numeric(y))

  id
}

