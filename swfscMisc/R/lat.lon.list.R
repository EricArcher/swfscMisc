#' @export lat.lon.list
#' @importFrom gpclib get.pts
#' 
#' @title Get List of Latitude and Longitude Coordinates
#' @description Create a list of coordinates on the inputed polygon.
#' 
#' @param area.poly area of a polygon. 
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}

lat.lon.list <- function(area.poly) {
  list(lon = get.pts(area.poly)[[1]]$x, lat = get.pts(area.poly)[[1]]$y)
}
