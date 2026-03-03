\name{OptimalBinning2D}
\alias{OptimalBinning2D}
\alias{OptimalPWBinning}
\alias{OptimalBinningSketch}
\alias{OptimalBinningUncertainty}
\title{Advanced Binning Model Constructors}
\description{
Constructors for 2D, piecewise, sketch/streaming, and uncertainty-aware
binning models.
}
\usage{
OptimalBinning2D(name_x, name_y, target_dtype = "binary")

OptimalPWBinning(name, target_dtype = "binary", degree = 1L)

OptimalBinningSketch(name, sample_size = 5000L)

OptimalBinningUncertainty(name, uncertainty_strategy = "mean")
}
\arguments{
  \item{name_x, name_y}{Feature names for two-dimensional binning.}
  \item{name}{Feature name.}
  \item{target_dtype}{Target type accepted by the specific model.}
  \item{degree}{Polynomial degree for piecewise binning model.}
  \item{sample_size}{Reservoir sample size for sketch binning.}
  \item{uncertainty_strategy}{Imputation strategy for uncertain values: one of \code{"mean"}, \code{"median"}, or \code{"mode"}.}
}
\value{A model object to be used with \code{fit()} or \code{partial_fit()}.}
\examples{
ob2 <- OptimalBinning2D("x1", "x2")
pw <- OptimalPWBinning("x")
sk <- OptimalBinningSketch("x")
un <- OptimalBinningUncertainty("x")
}
