#' @name na.count
#' @title Count NAs
#' @export na.count
#' @description Counts NAs in an object
#' @param x a vector, data.frame, or matrix
#' @return number of NAs in the object
#' @author Eric Archer <eric.archer@@noaa.gov>
#' 
#' @examples
#' x <- sample(c(1:10, NA), 30, replace = TRUE)
#' na.count(x)
#' x.df <- do.call(data.frame, lapply(1:4, function(i) sample(c(1:10, NA), 30, replace = TRUE)))
#' colnames(x.df) <- paste("X", 1:4, sep = "")
#' na.count(x.df)

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
