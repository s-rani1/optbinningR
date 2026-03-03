test_that("tutorial-level python parity checks pass when references available", {
  refs <- .extdata_path("python_tutorial_refs.json")
  if (!file.exists(refs)) {
    skip("python tutorial reference file not available")
  }
  if (!optbinningR:::.python_optbinning_available("ls")) {
    skip("python optbinning LocalSolver backend not available")
  }

  root <- testthat::test_path("..", "..")
  script <- file.path(root, "scripts", "compare_python_r_tutorials.R")
  if (!file.exists(script)) {
    skip("comparison script not available in installed test environment")
  }
  out <- suppressWarnings(system2("Rscript", script, stdout = TRUE, stderr = TRUE))
  json_path <- file.path(root, "inst", "extdata", "python_r_tutorials_comparison.json")
  expect_true(file.exists(json_path))
  cmp <- jsonlite::fromJSON(json_path)
  expect_true(isTRUE(cmp$all_match), info = paste(out, collapse = "\n"))
})
