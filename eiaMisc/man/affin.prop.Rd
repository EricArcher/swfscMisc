\name{affin.prop}
\alias{affin.prop}
\title{Affinity Propagation}
\usage{
affin.prop(sim.mat, num.iter = 100, stable.iter = 10, shared.pref = "min",
  lambda = 0.5)
}
\arguments{
  \item{sim.mat}{a similarity matrix between individuals to
  be clustered}

  \item{num.iter}{maximum number of iterations to attempt}

  \item{stable.iter}{number of sequential iterations for
  which consistent clustering is considered acceptable}

  \item{shared.pref}{type of shared preference to use. Can
  be one of "min", "median", or a numeric value}

  \item{lambda}{damping factor}
}
\value{
A matrix with one row per sample in 'sim.mat' and one
column for each iteration. Values in columns indicate
cluster assignment (arbitrary numbers) for each sample.
}
\description{
Runs the Affinity Propagation clustering algorithm of Frey
and Dueck, 2007.
}
\examples{
data(iris)
iris <- iris[sample(1:nrow(iris), floor(nrow(iris) / 2)), ]
droplevels(iris)
iris.sim <- -dist(iris[, -5])

iris.affin <- affin.prop(iris.sim, stable.iter = 5)
table(iris$Species, iris.affin[, ncol(iris.affin)])

# Increase the damping factor 'lambda'
iris.affin <- affin.prop(iris.sim, stable.iter = 5, lambda = 0.9)
table(iris$Species, iris.affin[, ncol(iris.affin)])
}
\author{
Eric Archer <eric.archer@noaa.gov>
}
\references{
Frey, B.J., and D. Dueck. 2007. Clustering by passing
messages between data points. Science 315:972-976
}

