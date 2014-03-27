#' @name row.col.page.fit
#' @title Calculate number of rows and columns to fit a page
#' @description Return the number of rows and columns for \code{n}
#' that best fits on a  page of size \code{width} x \code{height}
#' @param n number of items (e.g., plots) to fit on page
#' @param width,height dimensions of page
#' @export row.col.page.fit
#' @return A vector listing the number of rows and columns to use.
#' @author Eric Archer <eric.archer@@noaa.gov>

row.col.page.fit <- function(n, width = 8.5, height = 11) {
  n <- as.integer(n)
  if(any(n <= 0) | any(is.na(n))) stop("'n' must be a positive integer")
  nr <- 1:n
  nc <- n / nr
  ratio.diff <- (nc / nr) - (width / height)
  best <- which.min(abs(ratio.diff))
  c(num.rows = best, num.cols = ceiling(nc[best]))
}
