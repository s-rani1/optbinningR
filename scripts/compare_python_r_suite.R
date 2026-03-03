#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
solver <- if (length(args) >= 1L) args[[1L]] else "native"

root <- normalizePath(".", mustWork = TRUE)
source(file.path(root, "R", "optimal_binning.R"))
source(file.path(root, "R", "binning_table.R"))

suite_path <- file.path(root, "inst", "extdata", "python_optbinning_reference_suite.json")
out_path <- file.path(root, "inst", "extdata", "python_r_comparison_suite.json")

if (!file.exists(suite_path)) {
  stop("Missing suite artifacts. Run scripts/python_reference_suite.py first.", call. = FALSE)
}

suite <- fromJSON(suite_path, simplifyVector = FALSE)
results <- vector("list", length(suite$cases))

for (i in seq_along(suite$cases)) {
  case <- suite$cases[[i]]
  p <- case$params

  data_file <- file.path(root, "inst", "extdata", case$data_file)
  if (!file.exists(data_file)) {
    stop(paste("Missing case data file:", case$data_file), call. = FALSE)
  }
  df <- read.csv(data_file, check.names = FALSE)

  model <- fit(
    OptimalBinning(case$variable, dtype = case$dtype),
    x = df[[case$x_column]],
    y = df[[case$y_column]],
    algorithm = "optimal",
    solver = solver,
    prebinning_method = "cart",
    max_n_bins = if (is.null(p$max_n_bins)) NULL else as.integer(p$max_n_bins),
    max_n_prebins = as.integer(p$max_n_prebins),
    min_bin_size = as.numeric(p$min_prebin_size),
    min_bin_n_event = 1,
    min_bin_n_nonevent = 1,
    monotonic_trend = p$monotonic_trend,
    special_codes = case$special_codes
  )

  py_splits <- as.numeric(unlist(case$splits))
  if (case$dtype == "numerical") {
    r_splits <- as.numeric(model$breaks[2:(length(model$breaks) - 1)])
  } else {
    r_splits <- numeric(0)
  }

  min_len <- min(length(py_splits), length(r_splits))
  py_rates <- sort(as.numeric(unlist(case$event_rate_core)))

  model_core <- subset(model$bin_table, !(bin %in% c("Special", "Missing")))
  r_rates <- sort(as.numeric(model_core$event_rate))

  results[[i]] <- list(
    id = case$id,
    variable = case$variable,
    dtype = case$dtype,
    solver = solver,
    python_status = case$status,
    r_status = if (isTRUE(model$fitted)) "FITTED" else "FAILED",
    split_count_equal = length(py_splits) == length(r_splits),
    python_splits = py_splits,
    r_splits = r_splits,
    split_abs_diff_aligned = if (min_len > 0) as.numeric(abs(py_splits[1:min_len] - r_splits[1:min_len])) else numeric(),
    event_rate_count_equal = length(py_rates) == length(r_rates),
    event_rate_abs_diff = if (length(py_rates) == length(r_rates) && length(py_rates) > 0) as.numeric(abs(py_rates - r_rates)) else numeric(),
    iv_python = as.numeric(case$iv_total),
    iv_r = as.numeric(sum(model$bin_table$iv)),
    iv_abs_diff = abs(as.numeric(case$iv_total) - as.numeric(sum(model$bin_table$iv))),
    n_bins_python = as.integer(case$n_bins_core),
    n_bins_r = as.integer(nrow(model_core))
  )
}

all_match <- all(vapply(results, function(z) {
  split_ok <- if (z$dtype == "categorical") TRUE else {
    z$split_count_equal && (length(z$split_abs_diff_aligned) == 0 || max(z$split_abs_diff_aligned) < 1e-4)
  }
  rates_ok <- z$event_rate_count_equal && (length(z$event_rate_abs_diff) == 0 || max(z$event_rate_abs_diff) < 1e-8)
  bins_ok <- z$n_bins_python == z$n_bins_r
  split_ok && rates_ok && bins_ok && z$iv_abs_diff < 1e-6
}, logical(1)))

summary <- list(
  solver = solver,
  dataset = suite$dataset,
  all_match = all_match,
  cases = results
)

write_json(summary, out_path, auto_unbox = TRUE, pretty = TRUE)
cat(toJSON(summary, auto_unbox = TRUE, pretty = TRUE), "\n")
