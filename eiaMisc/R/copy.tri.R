#' @title Copy matrix triangles
#' @aliases copy.tri
#' @export copy.tri
#' @name copy.tri
#' @description Copy between lower left and upper right triangles of a matrix
#' @param mat a matrix
#' @param from,to triangle to copy from and to. Can be "lower" or "upper"
#' @return a matrix
#' @author Eric Archer <eric.archer@@noaa.gov>

copy.tri <- function(mat, from = "lower", to = "upper") {  
  if (!is.matrix(mat)) mat <- as.matrix(mat)
  if (nrow(mat) != ncol(mat)) stop("Matrix is not square.")
  new.mat <- mat
  from <- tolower(from)
  to <- toupper(to)
  if(charmatch(from, "lower", 0) && charmatch(to, "upper", 0)) {
    for (row in 1:(nrow(mat) - 1)) {
      for (col in (row + 1):nrow(mat)) new.mat[row, col] <- mat[col, row]
    }
  } else if(charmatch(from, "upper", 0) && charmatch(to, "lower", 0)) {
    for (row in 1:(nrow(mat) - 1)) {
      for (col in (row + 1):nrow(mat)) new.mat[col, row] <- mat[row, col]
    }
  }
  new.mat
}
