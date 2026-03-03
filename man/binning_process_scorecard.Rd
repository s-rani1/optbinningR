\name{BinningProcess}
\alias{BinningProcess}
\alias{Scorecard}
\alias{update_binning_process}
\alias{predict_proba}
\alias{predict_score}
\alias{scorecard_monitoring}
\alias{counterfactual_scorecard}
\title{Binning Process and Scorecard APIs}
\description{
Construct and operate multivariable binning processes and scorecard models,
including update and monitoring utilities.
}
\usage{
BinningProcess(variable_names, target_dtype = "binary", binning_fit_params = list())

Scorecard(binning_process, pdo = 20, odds = 50, base_points = 600)

update_binning_process(object, x, y, variables = NULL, ...)

predict_proba(object, x)

predict_score(object, x)

scorecard_monitoring(object, x_ref, x_new, y_ref = NULL, y_new = NULL, n_bins = 10L)

counterfactual_scorecard(
  object,
  x_one,
  target,
  direction = "auto",
  max_changes = 2L,
  max_combinations = 50000L
)
}
\arguments{
  \item{variable_names}{Character vector of variable names.}
  \item{target_dtype}{Target type, e.g. \code{"binary"} or \code{"continuous"}.}
  \item{binning_fit_params}{Named list of per-variable fit parameter overrides.}
  \item{binning_process}{A fitted or unfitted \code{BinningProcess} object.}
  \item{pdo, odds, base_points}{Score scaling parameters.}
  \item{object}{A fitted object.}
  \item{x}{Input data frame.}
  \item{y}{Target vector.}
  \item{variables}{Optional subset of variables to update.}
  \item{x_ref, x_new}{Reference and new/current population predictor data frames.}
  \item{y_ref, y_new}{Optional reference and new/current target vectors.}
  \item{n_bins}{Number of score buckets used for PSI.}
  \item{x_one}{One-row input data frame for counterfactual search.}
  \item{target}{Desired target prediction for counterfactual search.}
  \item{direction}{One of \code{"auto"}, \code{">="}, or \code{"<="}.}
  \item{max_changes}{Maximum number of feature changes in counterfactual search.}
  \item{max_combinations}{Maximum combinations evaluated for counterfactual search.}
  \item{...}{Additional arguments.}
}
\value{Object or summary output depending on function.}
\examples{
set.seed(1)
df <- data.frame(x1 = rnorm(200), x2 = runif(200))
y <- rbinom(200, 1, plogis(df$x1 - df$x2))

bp <- BinningProcess(c("x1", "x2"), target_dtype = "binary")
bp <- fit(bp, df, y, algorithm = "optimal", max_n_bins = 5)

sc <- Scorecard(bp)
sc <- fit(sc, df, y, algorithm = "optimal", max_n_bins = 5)

predict_proba(sc, df[1:5, ])
predict_score(sc, df[1:5, ])
}
