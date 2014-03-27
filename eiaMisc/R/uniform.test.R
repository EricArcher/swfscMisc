#' @name uniform.test
#' @title Uniform distribution test
#' @aliases uniform.test
#' @export uniform.test
#' @description Tests whether a histogram is significantly different from a uniform distribution
#' @usage uniform.test(hist.output, B = NULL)
#' @param hist.output output from a call to \code{hist}
#' @param B number of replicates for chi-squared permutation
#' @return result of chi-squared test
#' @author Eric Archer <eric.archer@@noaa.gov>

uniform.test <- function(hist.output, B = NULL) {
  break.diff <- diff(hist.output$breaks)
  probs <- break.diff / sum(break.diff)
  if (is.null(B)) {
    chisq.test(x = hist.output$counts, p = probs)
  } else {
    chisq.test(x = hist.output$counts, p = probs, simulate.p.value = TRUE, B = B)
  }
}
