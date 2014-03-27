#' @title Normalize a numeric vector
#' @name normalize
#' @export normalize
#' @description Normalize a numeric vector to have a mean of zero and a standard deviation of one
#' @param x a numeric vector
#' @return a numeric vector of the same length as x
#' @author Eric Archer <eric.archer@@noaa.gov>

normalize <- function(x) {
  if(!is.numeric(x) & !is.vector(x)) stop("'x' must be a numeric vector")
  x.mean <- mean(x, na.rm = TRUE)
  x.sd <- sd(x, na.rm = TRUE)
  (x - x.mean) / x.sd
}
