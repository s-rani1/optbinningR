#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

root <- normalizePath(".", mustWork = TRUE)
py_hint <- normalizePath(file.path(root, "..", ".venv-optbinning", "bin", "python"), mustWork = FALSE)
if (file.exists(py_hint)) {
  Sys.setenv(OPTBINNING_PYTHON = py_hint)
}
source(file.path(root, "R", "optimal_binning.R"))
source(file.path(root, "R", "binning_process_scorecard.R"))
source(file.path(root, "R", "tutorial_workflows.R"))

refs_path <- file.path(root, "inst", "extdata", "python_tutorial_refs.json")
if (!file.exists(refs_path)) {
  stop("Missing python_tutorial_refs.json. Run scripts/python_reference_tutorials.py first.", call. = FALSE)
}
refs <- fromJSON(refs_path, simplifyVector = FALSE)
solver_parity <- if (.python_optbinning_available("cp")) "python-cp" else "native"

# 1) LocalSolver exact split parity.
d <- read.csv(file.path(root, "inst", "extdata", "breast_cancer_mean_radius.csv"))
ls_engine_available <- isTRUE(refs$localsolver_breast_cancer$engine_available)
m_ls <- fit(
  OptimalBinning("mean radius"),
  d$x,
  d$y,
  algorithm = "optimal",
  solver = if (ls_engine_available) "localsolver" else "localsolver-native",
  prebinning_method = "cart",
  max_n_bins = 7,
  max_n_prebins = 20,
  min_bin_size = 0.05,
  monotonic_trend = "auto"
)
r_splits_ls <- as.numeric(m_ls$breaks[2:(length(m_ls$breaks) - 1)])
py_splits_ls <- if (ls_engine_available) as.numeric(unlist(refs$localsolver_breast_cancer$splits)) else numeric(0)

# 2) Large-scale profile parity.
dls <- read.csv(file.path(root, "inst", "extdata", "large_scale_synth.csv"))
m_lg <- fit(
  OptimalBinning("x"),
  dls$x,
  dls$y,
  algorithm = "optimal",
  solver = "native",
  profile = "large_scale",
  prebinning_method = "cart",
  max_n_bins = 8,
  max_n_prebins = 20,
  min_bin_size = 0.05,
  monotonic_trend = "auto"
)
r_splits_lg <- as.numeric(m_lg$breaks[2:(length(m_lg$breaks) - 1)])
py_splits_lg <- as.numeric(unlist(refs$large_scale_synth$splits))

# 3) FICO/Telco workflows on fixed datasets.
fico_train <- read.csv(file.path(root, "inst", "extdata", "fico_train.csv"), check.names = FALSE)
fico_update <- read.csv(file.path(root, "inst", "extdata", "fico_update.csv"), check.names = FALSE)
rf <- run_fico_tutorial(
  train_data = subset(fico_train, select = -y),
  y_train = fico_train$y,
  update_data = subset(fico_update, select = -y),
  y_update = fico_update$y,
  solver = solver_parity,
  profile = "standard",
  prebinning_method = "quantile",
  max_n_bins = 6,
  max_n_prebins = 20
)

telco_train <- read.csv(file.path(root, "inst", "extdata", "telco_train.csv"), check.names = FALSE)
telco_test <- read.csv(file.path(root, "inst", "extdata", "telco_test.csv"), check.names = FALSE)
rt <- run_telco_tutorial(
  train_data = subset(telco_train, select = -y),
  y_train = telco_train$y,
  test_data = subset(telco_test, select = -y),
  y_test = telco_test$y,
  solver = solver_parity,
  profile = "standard",
  prebinning_method = "quantile",
  max_n_bins = 6,
  max_n_prebins = 20
)

out <- list(
  localsolver = list(
    engine_available = ls_engine_available,
    split_count_equal = length(r_splits_ls) == length(py_splits_ls),
    split_abs_diff = if (ls_engine_available) as.numeric(abs(r_splits_ls - py_splits_ls)) else numeric(0),
    iv_abs_diff = if (ls_engine_available) abs(sum(m_ls$bin_table$iv) - as.numeric(refs$localsolver_breast_cancer$iv_total)) else NA_real_
  ),
  large_scale = list(
    split_count_equal = length(r_splits_lg) == length(py_splits_lg),
    split_abs_diff = as.numeric(abs(r_splits_lg - py_splits_lg)),
    iv_abs_diff = abs(sum(m_lg$bin_table$iv) - as.numeric(refs$large_scale_synth$iv_total))
  ),
  fico_workflow = list(
    train_auc_r = as.numeric(rf$train_auc),
    train_auc_py = as.numeric(refs$fico_workflow$train_auc),
    train_auc_abs_diff = abs(as.numeric(rf$train_auc) - as.numeric(refs$fico_workflow$train_auc))
  ),
  telco_workflow = list(
    test_auc_r = as.numeric(rt$test_auc),
    test_auc_py = as.numeric(refs$telco_workflow$test_auc),
    test_auc_abs_diff = abs(as.numeric(rt$test_auc) - as.numeric(refs$telco_workflow$test_auc))
  )
)

localsolver_ok <- if (isTRUE(out$localsolver$engine_available)) {
  isTRUE(out$localsolver$split_count_equal) &&
    max(out$localsolver$split_abs_diff) < 1e-4 &&
    out$localsolver$iv_abs_diff < 1e-6
} else {
  TRUE
}

out$all_match <- localsolver_ok &&
  isTRUE(out$large_scale$split_count_equal) &&
  max(out$large_scale$split_abs_diff) < 1e-4 &&
  out$large_scale$iv_abs_diff < 1e-4 &&
  out$fico_workflow$train_auc_abs_diff < 0.02 &&
  out$telco_workflow$test_auc_abs_diff < 0.02

write_json(out, file.path(root, "inst", "extdata", "python_r_tutorials_comparison.json"), auto_unbox = TRUE, pretty = TRUE)
cat(toJSON(out, auto_unbox = TRUE, pretty = TRUE))
cat("\n")
