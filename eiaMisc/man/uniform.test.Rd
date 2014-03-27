\name{uniform.test}
\alias{uniform.test}
\title{Uniform distribution test}
\usage{
uniform.test(hist.output, B = NULL)
}
\arguments{
  \item{hist.output}{output from a call to \code{hist}}

  \item{B}{number of replicates for chi-squared
  permutation}
}
\value{
result of chi-squared test
}
\description{
Tests whether a histogram is significantly different from a
uniform distribution
}
\author{
Eric Archer <eric.archer@noaa.gov>
}

