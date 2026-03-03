#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

args <- commandArgs(trailingOnly = TRUE)
solver <- if (length(args) >= 1L) args[[1L]] else "native"

root <- normalizePath(".", mustWork = TRUE)
source(file.path(root, "R", "optimal_binning.R"))
source(file.path(root, "R", "binning_table.R"))

csv_path <- file.path(root, "inst", "extdata", "breast_cancer_mean_radius.csv")
ref_path <- file.path(root, "inst", "extdata", "python_optbinning_reference.json")
out_path <- file.path(root, "inst", "extdata", "python_r_comparison.json")
r_out_path <- file.path(root, "inst", "extdata", "r_optbinning_reference.json")

if (!file.exists(csv_path) || !file.exists(ref_path)) {
  stop("Missing Python reference artifacts. Run scripts/python_reference_example.py first.", call. = FALSE)
}

d <- read.csv(csv_path)
py <- fromJSON(ref_path)

m <- fit(
  OptimalBinning(py$variable),
  x = d$x,
  y = d$y,
  algorithm = "optimal",
  solver = solver,
  max_n_bins = NULL,
  prebinning_method = "cart",
  max_n_prebins = 20,
  min_bin_size = 0.05,
  monotonic_trend = "auto"
)

r_res <- list(
  variable = py$variable,
  n = nrow(d),
  status = if (isTRUE(m$fitted)) "FITTED" else "FAILED",
  splits = as.numeric(m$breaks[2:(length(m$breaks) - 1)]),
  iv_total = as.numeric(sum(m$bin_table$iv)),
  n_bins = as.integer(m$n_bins),
  event_rate = as.numeric(m$bin_table$event_rate),
  woe = as.numeric(m$bin_table$woe)
)

write_json(r_res, r_out_path, auto_unbox = TRUE, pretty = TRUE)

py_splits <- as.numeric(py$splits)
r_splits <- as.numeric(r_res$splits)
min_len <- min(length(py_splits), length(r_splits))

cmp <- list(
  python_status = py$status,
  r_status = r_res$status,
  python_splits = py_splits,
  r_splits = r_splits,
  split_count_equal = length(py_splits) == length(r_splits),
  split_abs_diff_aligned = if (min_len > 0) as.numeric(abs(py_splits[1:min_len] - r_splits[1:min_len])) else numeric(),
  iv_python = as.numeric(py$iv_total),
  iv_r = as.numeric(r_res$iv_total),
  iv_abs_diff = abs(as.numeric(py$iv_total) - as.numeric(r_res$iv_total)),
  n_bins_python = as.integer(py$n_bins_core),
  n_bins_r = as.integer(r_res$n_bins),
  solver = solver
)

write_json(cmp, out_path, auto_unbox = TRUE, pretty = TRUE)
cat(toJSON(cmp, auto_unbox = TRUE, pretty = TRUE), "\n")
