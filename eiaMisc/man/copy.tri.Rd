\name{copy.tri}
\alias{copy.tri}
\title{Copy matrix triangles}
\usage{
copy.tri(x, from = "lower")
}
\arguments{
  \item{x}{a matrix}

  \item{from}{triangle to copy from. Can be "lower" or
  "upper".}
}
\value{
a matrix
}
\description{
Copy between lower left and upper right triangles of a
matrix
}
\examples{
x <- matrix(1:9, nrow = 3)
print(x)
copy.tri(x)
}
\author{
Eric Archer <eric.archer@noaa.gov>
}

