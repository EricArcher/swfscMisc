#' @title Transparent Colors
#' @description Return transparent form of a named color.
#'
#' @param col vector of colors as name, hexadecimal, or positive integer 
#'   (see \code{\link{col2rgb}}).
#' @param percent percent of transparency (0 = solid, 100 = transparent).
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @examples
#' 
#' pct <- seq(0, 100, by = 10)
#' plot(pct, pct, bg = transparent("red", pct), pch = 21, cex = 3, xlab = "X", ylab = "Y")
#' 
#' @export
#' 
transparent <- function(col, percent = 50) {
  rgb.val <- col2rgb(col)
  col.names <- paste(col, percent, sep = ".")
  rgb(
    red = rgb.val[1], green = rgb.val[2], blue = rgb.val[3], 
    alpha = (100 - percent) * 255 / 100, names = col.names, maxColorValue = 255
  )
}
  