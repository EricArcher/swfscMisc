#' @name overview
#' @title Overview
#' @aliases overview
#' @export overview
#' @description Gives summary of all objects in the workspace
#' @return a matrix the class, dimension, size, and number of NAs for each object
#' @author Eric Archer <eric.archer@@noaa.gov>

overview <- function() {
  prop <- function(x)
    list(class = data.class(x), dim = dim(x), size = object.size(x), NAs = na.count(x))
  do.call("rbind", eapply(.GlobalEnv, prop))
}
