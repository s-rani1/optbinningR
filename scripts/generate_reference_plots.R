#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Package 'pkgload' is required to run this script.", call. = FALSE)
  }
})

args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(script_file) == 0L) {
  script_file <- file.path("scripts", "generate_reference_plots.R")
}
root <- normalizePath(file.path(dirname(script_file), ".."), mustWork = TRUE)
setwd(root)
pkgload::load_all(root, quiet = TRUE)

out_dir <- file.path(root, "inst", "extdata", "reference_plots")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

save_plot <- function(path, width, height, expr) {
  grDevices::png(path, width = width, height = height)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
}

# Binary tutorial-like example (breast cancer mean radius)
bin_df <- read.csv(file.path(root, "inst", "extdata", "breast_cancer_mean_radius.csv"))
bin_model <- fit(
  OptimalBinning("mean radius"),
  x = bin_df[["x"]],
  y = bin_df[["y"]],
  algorithm = "optimal",
  solver = "native",
  prebinning_method = "cart",
  max_n_prebins = 20,
  min_bin_size = 0.05,
  monotonic_trend = "auto"
)
save_plot(file.path(out_dir, "binary_bin_ids_540x379.png"), 540, 379, {
  plot(bin_model, metric = "woe", style = "bin", add_special = TRUE, add_missing = TRUE, show_bin_labels = FALSE)
})

# Continuous tutorial example (Boston LSTAT -> MEDV)
data("Boston", package = "MASS")
cont_model <- fit(
  ContinuousOptimalBinning("LSTAT"),
  x = Boston[["lstat"]],
  y = Boston[["medv"]],
  algorithm = "optimal",
  prebinning_method = "cart",
  max_n_prebins = 20,
  min_bin_size = 0.05,
  monotonic_trend = "auto"
)
save_plot(file.path(out_dir, "continuous_default_540x379.png"), 540, 379, {
  plot(cont_model, metric = "mean", style = "bin", add_special = TRUE, add_missing = TRUE, show_bin_labels = FALSE)
})
save_plot(file.path(out_dir, "continuous_actual_540x379.png"), 540, 379, {
  plot(cont_model, metric = "mean", style = "actual")
})
save_plot(file.path(out_dir, "continuous_labels_540x379.png"), 540, 379, {
  plot(cont_model, metric = "mean", style = "bin", add_special = TRUE, add_missing = TRUE, show_bin_labels = TRUE)
})

# Multiclass tutorial example (wine ash)
multi_df <- read.csv(file.path(root, "inst", "extdata", "wine_multiclass.csv"), check.names = FALSE)
multi_model <- fit(
  MulticlassOptimalBinning("ash"),
  x = multi_df[["ash"]],
  y = multi_df[["target"]],
  algorithm = "optimal",
  prebinning_method = "cart",
  max_n_bins = 6,
  max_n_prebins = 20,
  min_bin_size = 0.05,
  monotonic_trend = "auto"
)
save_plot(file.path(out_dir, "multiclass_default_540x379.png"), 540, 379, {
  plot(multi_model, metric = "event_rate", style = "bin", add_special = TRUE, add_missing = TRUE, show_bin_labels = FALSE)
})
save_plot(file.path(out_dir, "multiclass_labels_760x490.png"), 760, 490, {
  plot(multi_model, metric = "event_rate", style = "bin", add_special = TRUE, add_missing = TRUE, show_bin_labels = TRUE)
})

cat("Reference plots generated in:", out_dir, "\n")
