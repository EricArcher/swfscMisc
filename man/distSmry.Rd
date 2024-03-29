% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distSmry.R
\name{distSmry}
\alias{distSmry}
\title{Distribution summary}
\usage{
distSmry(x, p = 0.95, ...)
}
\arguments{
\item{x}{vector of numerical values.}

\item{p}{percent of distribution to summarized by quantile interval (ci) and 
highest posterior density interval (hdi).}

\item{...}{arguments passed to \code{\link[modeest]{mlv}} to estimate the 
mode if \code{use.mlv} is \code{TRUE}.}
}
\description{
Summarize a numerical distribution.
}
\author{
Eric Archer \email{eric.archer@noaa.gov}
}
