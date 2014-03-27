#' @name one.arg
#' @title Does the function have just one argument
#' @param f a function
#' @export one.arg
#' @author Eric Archer <eric.archer@@noaa.gov>

one.arg <- function(f) length(formals(f)) == 1