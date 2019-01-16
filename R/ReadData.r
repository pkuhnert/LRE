#' @name Read-Data
#'
#' @title Read In Data
#'
#' @description These functions read in concentration
#' and flow data necessary for loads calculations. The two main functions
#' for reading in data are \code{ReadInData} and \code{ReadInDataFromR}. The
#' former function makes use of the internal \code{ReadC} and
#' \code{ReadQ} functions for reading in external text files of
#' concentration and flow respectively. The latter function assumes the
#' data have already been read in as a dataframe with two columns
#' (Date and concentration/flow) and will convert these into an object
#' suitable for analysis with
#' \code{LRE}.
#'
#' @usage
#' ReadInData(dirnm = NULL, filenm, Cnames, OmitCzeros = FALSE)
#' ReadInDataFromR(x.C, x.Q)
#'
#' @param dirnm The directory name where the files are stored,
#' e.g. 'C:/Data/'
#' @param filenm The name of the csv files where the concentration
#' and flow data are stored. This should be a name common to
#' both files with the extension \code{_C} and \code{_Q} representing
#' the location of the concentration and flow data respectively.
#' For example, \code{filenm = "test"} indicates that there is a
#' \code{test_C.csv} and a \code{test_Q.csv} located in the specified
#' \code{dirnm} directory.
#' @param Cnames name of concentration variable
#' @param OmitCzeros a logical value indicating whether zeros should
#' be omitted in the concentration record. If missing, it is assumed
#' that no zeros should be omitted from the record if they exist.
#' @param x.C concentration dataframe
#' @param x.Q flow dataframe

#'
#' @details
#' The \code{ReadInData} function reads in two comma delimited text files
#' using the \code{\link{read.csv}} function. The first is a
#' concentration dataset containing two columns: date and concentration
#' measured in mg/L. The second is a flow dataset containing two columns:
#' date and flow measured in cumecs.
#'
#' The concentration and flow files will have a name common to both and
#' are distinguished by a \code{_C} for concentration and a \code{Q}
#' for flow. These need to be .csv files.
#'
#' By default, if zeros exist in the concentration record, they will
#' be left in the record. If they are to be removed then the
#' \code{OmitCzeros} argument will need to be set to \code{TRUE}.
#'
#' The \code{ReadInDataFromR} function reads in two R objects that are dataframes.
#' The first is a concentration dataset containing two columns:
#' date and concentration measured in mg/L. The second is a flow
#' dataset containing two columns: date and flow measured in cumecs.
#'
#' @return A \code{list} of class 'data' containing:
#' \code{Q} Observed flow dataset
#' \code{Conc} Observed concentration dataset
#' Standard \code{summary} and \code{plot} functions apply.
#'
#'
#' @references Kuhnert, P.M., Henderson, B.L., Lewis, S.E.,
#' Bainbridge, Z.T., Wilkinson, S.N. and Brodie, J.E. (2012)
#' Quantifying total suspended sediment export from the Burdekin
#' River catchment using the loads regression estimator tool,
#' Water Resources Research, 48, W04533,doi:10.1029/2011WR011080.
#'
#' @note Concentrations and flows measured in other formats will
#' need to be converted prior to using this function and
#' subsequent functions of the \code{LRE} package.
#'
#' @seealso \code{read.csv}, \code{ReadInData}
#'
#' @keywords read, loads
#'
#' @examples
#' # Reading in data from a file
#' # This function assumes you will have two csv files:
#' # burdR_Q.csv and burdR_C.csv representing
#' # flow and concentration files respectively
#' # The following script is not executed:
#' \dontrun{
#'  # Reading in data from external files
#'  library(LRE)
#'  burdR <- ReadInData(dirnm = "C:/Files/", filenm = "burdR",
#'             Cnames = "TSS")
#'  }
#'
#' # Reading in data already stored in R
#' library(LRE)
#' burdR <- ReadInDataFromR(x.C = burdRC, x.Q = burdRQ)
#' plot(burdR)
#' summary(burdR)
#' hist(burdR)
#'
#'  \dontrun{
#'  # Reading in data from external files
#'  library(LRE)
#'  burdR <- ReadInData(dirnm = "C:/Files/", filenm = "burdR",
#'             Cnames = "TSS")
#'
#'
#'  }


#' @rdname Read-Data
#' @export
ReadInData <- function(dirnm = NULL, filenm, Cnames, OmitCzeros = FALSE){

  if(missing(filenm))
    stop("Please supply a file name for reading in data e.g. 'burdR'. This will assume that
         burdR_Q.csv and burdR_C.csv are being supplied.\n")

  if(missing(Cnames))
    stop("Please supply a label for your concentration column e.g. 'TSS'.\n")

  if(!is.null(dirnm))
    fn <- paste(dirnm, filenm, sep = "")
  else
    fn <- filenm
  Q <- ReadQ(fn = paste(fn, "_Q.csv", sep = ""))
  Conc <- ReadC(fn = paste(fn, "_C.csv", sep = ""), Cnames = Cnames, OmitCzeros)
  if(OmitCzeros){
    zeroID <- Conc[,-1] != 0
    Conc <- Conc[zeroID,]
    cat(length(zeroID[!zeroID]), " concentration records deleted.\n")
  }
  out <- list(Q = Q, Conc = Conc)
  oldClass(out) <- "data"
  out
}


#' @rdname Read-Data
#' @export
ReadInDataFromR <- function(x.C, x.Q){

  if(missing(x.C))
    stop("Missing concentration data. \n")
  if(missing(x.Q))
    stop("MIssing flow data. \n")

  if(ncol(x.C) !=2)
    stop("Your concentration data should be a dataframe or matrix with
         two columns. The first should house the Date and the second should
         house the concentration")

  if(ncol(x.Q) !=2)
    stop("Your flow data should be a dataframe or matrix with
         two columns. The first should house the Date and the second should
         house the flow")

  x.conc <- x.C
  x.flow <- x.Q
  names(x.flow)[2] <- "Flow"
  site <- list(Q = x.flow, Conc = x.conc)
  # Omit NAs
  site$Conc <- site$Conc[!is.na(site$Conc[,2]),]
  site$Q <- site$Q[!is.na(site$Q$Flow),]
  class(site) <- "data"

  site
}



