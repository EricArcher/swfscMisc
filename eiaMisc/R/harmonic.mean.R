#' @name harmonic.mean
#' @title Harmonic Mean Function
#' @description Calculates the harmonic mean of a vector
#' @export harmonic.mean
#' @param x vector
#' @return a number giving the harmonic mean of a vector
#' @author Eric Archer <eric.archer@@noaa.gov>
#'  
#' @examples
#' x <- rlnorm(100)
#' mean(x)
#' median(x)
#' harmonic.mean(x)

harmonic.mean <- function(x) {
  #
  # Calculate harmonic mean of vector x
  # If any x < 0, then approximation used
  #
  #   9/15/2010
  
  x <- na.omit(x)
  if(length(x) == 0) return(NA)
  hm <- if(all(x > 0)) {
    length(x) / sum(1 / x)
  } else {
    inv.mean.x <- 1 / mean(x)
    var.x <- var(x)
    1 / (inv.mean.x + var.x * inv.mean.x ^ 3)
  }
  ifelse(is.nan(hm), NA, hm)
}
