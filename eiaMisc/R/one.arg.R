#' @title Does the function have just one argument?
#' @export one.arg
#' @param f a function
#' @author Eric Archer <eric.archer@@noaa.gov>
#' 
#' @examples
#' one.arg(mean)
#' one.arg(one.arg)

one.arg <- function(f) length(formals(f)) == 1