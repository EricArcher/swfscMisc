#' @name geometric.mean
#' @title Geometric Mean Function
#' @description Calculates the geometric mean of a vector
#' @export geometric.mean
#' @param x vector
#' @return a number giving the geometric mean of a vector
#' @author Eric Archer <eric.archer@@noaa.gov>
#' 
#' @examples
#' x <- rlnorm(100)
#' mean(x)
#' median(x)
#' geometric.mean(x)

geometric.mean <- function(x) {
  x <- na.omit(x)
  if(length(x) == 0) return(NA)
  prod(x) ^ (1 / length(x))
}