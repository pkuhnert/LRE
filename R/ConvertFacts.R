#' @rdname LRE-predict-Internal
ConvertFacts <- function(samp.unit){

  if(samp.unit == "day"){
     K.loads <- 24*60*60/1000000/1000000 #Mt
     K.flow <- 24*60*60/1000
  }
  else if(samp.unit == "hour"){ # mg/l x m3/1000 = 1kg
     K.loads <-  60*60/1000000/1000000
     K.flow <- 60*60/1000
  }
  else stop("Sampling unit not implemented. Only day and hour are implemented.\n")

  list(K.loads = K.loads, K.flow = K.flow)
}

