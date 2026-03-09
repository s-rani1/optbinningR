\name{run_fico_tutorial}
\alias{run_fico_tutorial}
\alias{run_telco_tutorial}
\title{Tutorial Workflow Helpers}
\description{
Run end-to-end workflow helpers inspired by FICO and Telco tutorials.
}
\usage{
run_fico_tutorial(
  n_train = 2500L,
  n_update = 1200L,
  train_data = NULL,
  y_train = NULL,
  update_data = NULL,
  y_update = NULL,
  solver = "native",
  profile = "standard",
  prebinning_method = "quantile",
  max_n_bins = 6L,
  max_n_prebins = 20L
)

run_telco_tutorial(
  n = 3000L,
  train_data = NULL,
  y_train = NULL,
  test_data = NULL,
  y_test = NULL,
  solver = "native",
  profile = "standard",
  prebinning_method = "quantile",
  max_n_bins = 6L,
  max_n_prebins = 20L
)
}
\arguments{
  \item{n_train, n_update}{Sample sizes for synthetic generation when data is not supplied.}
  \item{n}{Total sample size used for synthetic Telco generation when data is not supplied.}
  \item{train_data, update_data, test_data}{Optional input data frames.}
  \item{y_train, y_update, y_test}{Optional target vectors.}
  \item{solver}{Solver backend selection.}
  \item{profile}{Optimization profile.}
  \item{prebinning_method}{Prebinning method.}
  \item{max_n_bins, max_n_prebins}{Binning controls.}
}
\value{A list with fitted artifacts and summary metrics.}
\examples{
\donttest{
res_f <- run_fico_tutorial(n_train = 300, n_update = 150)
res_t <- run_telco_tutorial(n = 600)
}
}
