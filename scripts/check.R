#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
run_full_check <- "--full" %in% args
pkg_dir <- normalizePath(".", mustWork = TRUE)

message("[check] Loading package from: ", pkg_dir)

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required. Install with install.packages('pkgload').", call. = FALSE)
}
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required. Install with install.packages('testthat').", call. = FALSE)
}

pkgload::load_all(pkg_dir, quiet = TRUE)

message("[check] Running testthat tests...")
testthat::test_dir(file.path(pkg_dir, "tests", "testthat"), stop_on_failure = TRUE)
message("[check] Tests passed.")

if (run_full_check) {
  message("[check] Running R CMD check...")
  if (!requireNamespace("rcmdcheck", quietly = TRUE)) {
    stop("Package 'rcmdcheck' is required for --full. Install with install.packages('rcmdcheck').", call. = FALSE)
  }

  res <- rcmdcheck::rcmdcheck(
    path = pkg_dir,
    args = c("--no-manual"),
    error_on = "warning",
    check_dir = file.path(pkg_dir, "check")
  )

  message("[check] R CMD check status: ", res$status)
}

message("[check] Done.")
