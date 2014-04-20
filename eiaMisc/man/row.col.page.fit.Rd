\name{row.col.page.fit}
\alias{row.col.page.fit}
\title{Calculate number of rows and columns to fit a page}
\usage{
row.col.page.fit(n, width = 8.5, height = 11)
}
\arguments{
  \item{n}{number of items (e.g., plots) to fit on page}

  \item{width,height}{dimensions of page}
}
\value{
A vector listing the number of rows and columns to use.
}
\description{
Return the number of rows and columns for \code{n} that
best fits on a page of size \code{width} x \code{height}
}
\examples{
row.col.page.fit(9)
row.col.page.fit(9, width = 10, height = 10)
}
\author{
Eric Archer <eric.archer@noaa.gov>
}

