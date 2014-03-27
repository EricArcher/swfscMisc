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

  \item{lambda}{parameter}
}
\value{
matrix with one row per sample in 'sim.mat' and one column
for each iteration. Values in columns indicate cluster
assignment for sample
}
\description{
Runs the Affinity Propagation clustering algorithm of Frey
and Dueck, 2007.
}
\author{
Eric Archer <eric.archer@noaa.gov>
}

