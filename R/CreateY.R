#' @name CreateY
#'
#' @title Creates a categorical year variable
#'
#' @description Creates a categorical variable of year
#' type that is either based on a water year or financial
#' year.
#'
#' @param Date date vector
#' @param Ytype year type, either 'WY' (water year) or 'FY'
#' financial year
#'
#' @export

CreateY <- function(Date, Ytype){

  if(missing(Date))
    stop("No date vector specified. \n")
  if(missing(Ytype))
    stop("Ytype needs to be specified. Either financial year (FY) or water
           year (WY) are accepted.\n")

  if(!(Ytype == "WY" | Ytype == "FY"))
    stop("Either WY or FY need to be specified for Ytype.")


   dateL <- as.POSIXlt(Date)
   if(all(is.na(dateL)))
     stop("Date has missing values.\n")

   year <- dateL$year + 1900
   mon <- dateL$mon + 1
   Y <- rep("", length(Date))
   if(Ytype == "FY")
      id <- (mon <= 6)
   else if(Ytype == "WY")
      id <- (mon <= 9)

   Y[id] <- paste(year[id]-1, "/", year[id], sep = "")
   Y[!id] <- paste(year[!id], "/", year[!id]+1, sep = "")


   Y
}

