#!/usr/bin/env Rscript

root <- normalizePath(".", mustWork = TRUE)
source(file.path(root, "R", "optimal_binning.R"))
source(file.path(root, "R", "binning_process_scorecard.R"))
source(file.path(root, "R", "tutorial_workflows.R"))

res <- run_fico_tutorial()
cat("FICO tutorial summary\n")
cat(sprintf("  Train AUC: %.6f\n", res$train_auc))
cat(sprintf("  PSI total: %.6f\n", res$monitoring$psi_total))
cat(sprintf("  AUC delta: %.6f\n", res$monitoring$auc_delta))
