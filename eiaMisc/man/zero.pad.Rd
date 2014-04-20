\name{zero.pad}
\alias{zero.pad}
\title{Zero pad integers}
\usage{
zero.pad(x)
}
\arguments{
  \item{x}{a vector of integers}
}
\description{
Zero pad integers
}
\examples{
x <- c(0, 1, 3, 4, 10)
zero.pad(x)
x <- c(x, 11, 12, 100, 1000)
zero.pad(x)
}
\author{
Eric Archer <eric.archer@noaa.gov>
}

