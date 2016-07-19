#' @title Rounding Numbers for Data Frames
#' @description Rounds numeric columns in data.frames
#'
#' @param x a data.frame with numeric columns.
#' @param digits integer indicating the number of decimal places (\code{round})
#'   or significant digits (\code{signif}) to be used. See \code{\link[base]{round}} for 
#'   more details.
#' @param ... arguments to be passed to methods.
#'
#' @details Takes a data.frame and returns a data.frame with the specified function 
#'   applied to each numeric column.
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @seealso \code{\link[base]{round}}
#'
#' @examples
#' data(mtcars)
#' 
#' round(mtcars, 0)
#' 
#' signif(mtcars, 2)
#'
#' @name round
#' @aliases ceiling.data.frame floor.data.frame trunc.data.frame round.data.frame signif.data.frame ceiling floor trunc round siginif
#' @importFrom methods setMethod
#' 
NULL

#' @rdname round
#' @export
#' 
setMethod("ceiling", "data.frame", function(x) {
  for(i in 1:ncol(x)) {
    if(is.numeric(x[[i]])) x[[i]] <- ceiling(x[[i]])
  }
  x
})

#' @rdname round
#' @export
#' 
setMethod("floor", "data.frame", function(x) {
  for(i in 1:ncol(x)) {
    if(is.numeric(x[[i]])) x[[i]] <- floor(x[[i]])
  }
  x
})

#' @rdname round
#' @export
#' 
setMethod("trunc", "data.frame", function(x, ...) {
  for(i in 1:ncol(x)) {
    if(is.numeric(x[[i]])) x[[i]] <- ceiling(x[[i]], ...)
  }
  x
})

#' @rdname round
#' @export
#' 
setMethod("round", "data.frame", function(x, digits = 0) {
  for(i in 1:ncol(x)) {
    if(is.numeric(x[[i]])) x[[i]] <- round(x[[i]], digits = digits)
  }
  x
})

#' @rdname round
#' @export
#' 
setMethod("signif", "data.frame", function(x, digits = 6) {
  for(i in 1:ncol(x)) {
    if(is.numeric(x[[i]])) x[[i]] <- signif(x[[i]], digits = digits)
  }
  x
})