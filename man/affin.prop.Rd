% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/affin.prop.R
\name{affin.prop}
\alias{affin.prop}
\title{Affinity Propagation}
\usage{
affin.prop(
  sim.mat,
  num.iter = 100,
  stable.iter = 10,
  shared.pref = "min",
  lambda = 0.5
)
}
\arguments{
\item{sim.mat}{a similarity matrix between individuals to be clustered.}

\item{num.iter}{maximum number of iterations to attempt.}

\item{stable.iter}{number of sequential iterations for which consistent clustering is considered acceptable.}

\item{shared.pref}{type of shared preference to use. Can be one of "min", "median", or a numeric value.}

\item{lambda}{damping factor.}
}
\value{
A matrix with one row per sample in 'sim.mat' and one column for each iteration. Values in columns indicate 
cluster assignment (arbitrary numbers) for each sample.
}
\description{
Runs the Affinity Propagation clustering algorithm of Frey and Dueck, 2007.
}
\examples{
data(iris)

# Take 75 random iris rows for example
iris <- iris[sample(1:nrow(iris), 75), ]
iris <- droplevels(iris)

iris.sim <- -dist(iris[, -5])

iris.affin <- affin.prop(iris.sim, stable.iter = 5)
table(iris$Species, iris.affin[, ncol(iris.affin)])

}
\references{
Frey, B.J., and D. Dueck. 2007. Clustering by passing messages between data points. Science 315:972-976
}
\author{
Eric Archer \email{eric.archer@noaa.gov}
}
