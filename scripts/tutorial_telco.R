#!/usr/bin/env Rscript

root <- normalizePath(".", mustWork = TRUE)
source(file.path(root, "R", "optimal_binning.R"))
source(file.path(root, "R", "binning_process_scorecard.R"))
source(file.path(root, "R", "tutorial_workflows.R"))

res <- run_telco_tutorial()
cat("Telco tutorial summary\n")
cat(sprintf("  Test AUC: %.6f\n", res$test_auc))
cat(sprintf("  Train size: %d\n", res$n_train))
cat(sprintf("  Test size: %d\n", res$n_test))
