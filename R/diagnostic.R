#' @name diagnostic
#'
#' @description S3 method for evaluating diagnostics of a fitted LRE object
#'
#' @importFrom "stats" "resid"
#' @importFrom "stats" "lag"
#'
#' @export
diagnostic <- function(object)
  UseMethod("diagnostic")

