\name{invLogOdds}
\alias{invLogOdds}
\alias{invOdds}
\alias{logOdds}
\alias{odds}
\title{Odds conversion}
\usage{
invLogOdds(x)

invOdds(x)

logOdds(x)

odds(x)
}
\arguments{
  \item{x}{a numeric vector of probabilities (0..1), odds
  (0..Inf), or log.odds (-Inf..Inf)}
}
\description{
\code{odds} converts probability to odds \cr \code{logOdds}
converts odds to log-odds \cr \code{invOdds} converts odds
to probability \cr \code{invLogOdds} converts log-odds to
odds \cr
}
\author{
Eric Archer <eric.archer@noaa.gov>
}

