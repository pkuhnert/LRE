#' @name hist
#'
#' @title histogram
#'
#' @description Produce a histogram of the raw data
#'
#' @param object data object
#'
#'
#' @export

hist <- function(object)
  UseMethod("hist")

