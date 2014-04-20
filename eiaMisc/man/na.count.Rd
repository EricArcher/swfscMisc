\name{na.count}
\alias{na.count}
\title{Count NAs}
\usage{
na.count(x)
}
\arguments{
  \item{x}{a vector, data.frame, or matrix}
}
\value{
number of NAs in the object
}
\description{
Counts NAs in an object
}
\examples{
x <- sample(c(1:10, NA), 30, replace = TRUE)
na.count(x)
x.df <- do.call(data.frame, lapply(1:4, function(i) sample(c(1:10, NA), 30, replace = TRUE)))
colnames(x.df) <- paste("X", 1:4, sep = "")
na.count(x.df)
}
\author{
Eric Archer <eric.archer@noaa.gov>
}

