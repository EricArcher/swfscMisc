#' \code{swfscMisc} package
#'
#' SWFSC Miscellaneous Functions
#' 
#' @docType package
#' @name swfscMisc
#' @importFrom magrittr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# #' Pipe operator
# #'
# #' @name %>%
# #' @rdname pipe
# #' @keywords internal
# #' @export
# #' @importFrom magrittr %>%
# #' @usage lhs \%>\% rhs
# NULL