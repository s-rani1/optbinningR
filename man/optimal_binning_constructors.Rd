\name{OptimalBinning}
\alias{OptimalBinning}
\alias{MulticlassOptimalBinning}
\alias{ContinuousOptimalBinning}
\title{Create Optimal Binning Model Objects}
\description{
Constructors for one-dimensional optimal binning model objects for binary,
multiclass, and continuous targets.
}
\usage{
OptimalBinning(name, dtype = "numerical")

MulticlassOptimalBinning(name, dtype = "numerical")

ContinuousOptimalBinning(name, dtype = "numerical")
}
\arguments{
  \item{name}{Feature name.}
  \item{dtype}{Feature type. Use \code{"numerical"} or \code{"categorical"} where supported.}
}
\value{
A model object to be used with \code{fit()}.
}
\examples{
ob <- OptimalBinning("x")
mc <- MulticlassOptimalBinning("x")
ct <- ContinuousOptimalBinning("x")
}
