#' @name na.count
#' @title Count NAs
#' @aliases na.count
#' @export na.count
#' @description Counts NAs in an object
#' @usage na.count(x)
#' @param x a vector, data.frame, or matrix
#' @return number of NAs in the object
#' @author Eric Archer <eric.archer@@noaa.gov>

na.count <- function(x) {
  if (is.vector(x)) return (sum(is.na(x)))
  if (is.data.frame(x) | is.matrix(x)) {
    col.count <- sapply(1:ncol(x), function(i) sum(is.na(x[, i])))
    names(col.count) <- colnames(x)
    col.count
  } else {
    NA
  }
}
