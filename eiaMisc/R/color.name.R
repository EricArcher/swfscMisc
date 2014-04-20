#' @name color.name
#' @title Color name
#' @export color.name
#' @description Return the name of a color listed given the number
#' @usage color.name(i)
#' @param i integer specifying color 
#' @return character value of 'i' color 
#' @author Eric Archer <eric.archer@@noaa.gov>

color.name <- function(i) colors()[i]