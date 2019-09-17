#' @name LRE-Read-Internal
#'
#' @title Internal functions for \code{ReadInData} function.
#'
#' @description Internal functions for reading in data
#'
#'
#' @param fn file name
#' @param format format for the date 
#' @param Cnames concentration column label
#' @param OmitCzeros a logical value indicating whether zeros should
#' be omitted in the concentration record. If missing, it is assumed
#' that no zeros should be omitted from the record if they exist.
#' @param dt character representing the date
#'
#' @importFrom "utils" "read.csv"
#' @importFrom "stats" "na.omit"
#'
NULL

#' @rdname LRE-Read-Internal
ReadQ <- function(fn, format){

  dat.Q <- read.csv(fn)
  if(ncol(dat.Q) > 2){
    id.na <- (1:nrow(dat.Q))[is.na(dat.Q[,3]) | dat.Q[,1] == ""]

    datetime <- as.vector(paste(dat.Q[,1], dat.Q[,2]) )[-id.na]
    Q <- dat.Q[-id.na,3]
  }
  else{
    id.na <- (1:nrow(dat.Q))[is.na(dat.Q[,2]) | dat.Q[,1] == ""]
    if(length(id.na) == 0){
       datetime <- as.vector(dat.Q[,1])
       Q <- dat.Q[,2]
    }
    else{
      datetime <- as.vector(dat.Q[-id.na,1])
    Q <- dat.Q[-id.na,2]
    }
  }


  datetime <- ExtractDT(datetime, format = format)

  dat.Q <- data.frame(Date = datetime, Flow = Q)
  dat.Q <- dat.Q[!is.na(dat.Q$Date),]
  if(any(is.na(dat.Q)))
    warning("Missing values appearing in flow record.\n")
  if(any(dat.Q[,-1] == 0))
    warning("Zeros appearing in flow record. These will be imputed by a small number so logs can be taken.\n")
  dat.Q
}

#' @rdname LRE-Read-Internal
ReadC <-function(fn, Cnames, OmitCzeros = FALSE, format){

  dat.C <- read.csv(fn)
  datetime <- as.vector(dat.C[,1])
  Conc <- as.numeric(as.vector(dat.C[,2]))

  datetime <- ExtractDT(datetime, format = format)

  dat.C <- data.frame(Date = datetime, Conc)
  names(dat.C)[2] <- Cnames
  dat.C <- dat.C[!is.na(dat.C$Date),]
  if(any(is.na(dat.C))){
    warning("Missing values appearing in concentration record. Deleting records with NAs.\n")
    dat.C <- na.omit(dat.C)
  }
  if(!OmitCzeros){
    if(any(dat.C[,-1] == 0))
      warning("Zeros appearing in concentration record. May want to omit these records from the analysis. To
              delete, re-run function with OmitCzeros = TRUE.\n")
  }

  if(any(dat.C[,-1] < 0)){
    warning("Negative values appearing in concentration record. These will be deleted. \n")
    id <- apply(dat.C[,-1], 1, function(x) x < 0)
    dat.C <- dat.C[!id,]
  }
  dat.C
  }

#' @rdname LRE-Read-Internal
ExtractDT <- function(dt, format = "%Y-%m-%d"){

    
    datetime <- as.POSIXct(dt, format =  format, tz = "GMT")

    if(all(is.na(datetime)))
       stop("Date not recognised. Please check date.\n")
    datetime

}

