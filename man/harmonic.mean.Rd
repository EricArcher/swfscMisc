% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harmonic.mean.R
\name{harmonic.mean}
\alias{harmonic.mean}
\title{Harmonic Mean}
\usage{
harmonic.mean(x, na.rm = FALSE)
}
\arguments{
\item{x}{a numeric vector.}

\item{na.rm}{a logical value indicating whether NA values should be stripped 
before the computation proceeds.}
}
\description{
Calculate the harmonic mean of a set of numbers.
}
\examples{
x <- rlnorm(100)
mean(x)
median(x)
harmonic.mean(x)

}
\author{
Eric Archer \email{eric.archer@noaa.gov}
}
