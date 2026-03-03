\name{fit}
\alias{fit}
\alias{fit_transform}
\alias{transform}
\alias{partial_fit}
\alias{information}
\alias{analysis}
\title{Fit, Transform, and Inspect Binning Models}
\description{
Generic APIs to fit models, transform variables, and inspect fitted binning
objects.
}
\usage{
fit(object, ...)

fit_transform(
  object,
  x,
  y,
  max_n_bins = 10L,
  algorithm = "quantile",
  prebinning_method = "cart",
  max_n_prebins = 20L,
  min_bin_size = 0.05,
  min_bin_n_event = 1L,
  min_bin_n_nonevent = 1L,
  monotonic_trend = "none",
  special_codes = NULL,
  solver = "native",
  profile = "standard"
)

transform(object, x, metric = NULL, ...)

partial_fit(object, x, y)

information(object, print_level = 1L)

analysis(object)
}
\arguments{
  \item{object}{A model object.}
  \item{x}{Predictor vector or data structure accepted by the model.}
  \item{y}{Target vector.}
  \item{max_n_bins}{Maximum final bins.}
  \item{algorithm}{Binning algorithm.}
  \item{prebinning_method}{Prebinning method for optimal mode.}
  \item{max_n_prebins}{Maximum pre-bins for the \code{"optimal"} algorithm.}
  \item{min_bin_size}{Minimum bin size as count or fraction.}
  \item{min_bin_n_event}{Minimum number of events required per bin (native solver).}
  \item{min_bin_n_nonevent}{Minimum number of non-events required per bin (native solver).}
  \item{monotonic_trend}{Monotonic trend constraint.}
  \item{special_codes}{Optional values treated as \code{Special} bin.}
  \item{solver}{Solver backend for \code{algorithm = "optimal"}.}
  \item{profile}{Optimization profile (\code{"standard"} or \code{"large_scale"}).}
  \item{metric}{Transformation metric. Common values include \code{"woe"}, \code{"event_rate"}, and \code{"bins"}.}
  \item{print_level}{Verbosity level for \code{information()}.}
  \item{...}{Additional model-specific arguments.}
}
\value{
Depends on the method:
\itemize{
  \item \code{fit()}: fitted object.
  \item \code{fit_transform()}: list with fitted model and transformed values.
  \item \code{transform()}: transformed vector.
  \item \code{partial_fit()}: updated object.
  \item \code{information()}: summary list (invisibly).
  \item \code{analysis()}: diagnostics list.
}
}
\examples{
x <- c(1, 2, 3, 4, 5, 6)
y <- c(0, 0, 0, 1, 1, 1)

m <- fit(OptimalBinning("x"), x, y, algorithm = "optimal", max_n_bins = 3)
transform(m, x)
transform(m, x, metric = "bins")
information(m)
analysis(m)
}
