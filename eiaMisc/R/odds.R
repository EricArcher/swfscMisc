#' @rdname odds
#' @export odds logOdds invOdds invLogOdds
#' @title Odds conversion
#' @description \code{odds} converts probability to odds \cr
#' \code{logOdds} converts odds to log-odds \cr
#' \code{invOdds} converts odds to probability \cr
#' \code{invLogOdds} converts log-odds to odds \cr
#' @param x a numeric vector of probabilities (0..1), odds (0..Inf), or log.odds (-Inf..Inf) 
#' @author Eric Archer <eric.archer@@noaa.gov>

odds <- function(x) ifelse(x < 0 | x > 1, as.numeric(NA), x / (1 - x))