\name{central.quantile}
\alias{central.quantile}
\title{Central quantile}
\usage{
central.quantile(x, pct = 0.95)
}
\arguments{
  \item{x}{numeric vector}

  \item{pct}{central percentile desired}
}
\value{
a two element vector giving the lower and upper quantiles
}
\description{
Upper and lower values of central quantile
}
\examples{
x <- runif(1000)
central.quantile(x)
central.quantile(x, pct = 0.75)
}
\author{
Eric Archer <eric.archer@noaa.gov>
}

