test_that("multiclass parity surface arguments and aliases are available", {
  d <- read.csv(.extdata_path("wine_multiclass.csv"), check.names = FALSE)

  m <- fit(
    MulticlassOptimalBinning("ash"),
    x = d[["ash"]],
    y = d[["target"]],
    algorithm = "optimal",
    solver = "cp",
    prebinning_method = "cart",
    max_n_bins = 6,
    max_n_prebins = 20,
    min_bin_size = 0.05,
    monotonic_trend = list("ascending", "auto", "none"),
    min_event_rate_diff = 0.0,
    max_pvalue = 0.95,
    max_pvalue_policy = "consecutive",
    user_splits = c(2.15, 2.25, 2.31, 2.60, 2.65),
    user_splits_fixed = rep(TRUE, 5),
    verbose = TRUE
  )

  expect_true(m$fitted)
  expect_equal(m$target_dtype, "multiclass")
  expect_equal(m$status, "OPTIMAL")
  expect_true(is.numeric(m$splits))
  expect_true(length(m$splits) >= 1)
  expect_gt(length(grep("^event_rate_class_", names(m$bin_table))), 0)

  info <- information(m, print_level = 0)
  expect_true(is.list(info))
  expect_equal(info$status, "OPTIMAL")

  an <- analysis(m)
  expect_true(is.list(an))
  expect_equal(an$target, "multiclass")
})

test_that("transform metric bins works for multiclass and continuous", {
  w <- read.csv(.extdata_path("wine_multiclass.csv"), check.names = FALSE)
  m <- fit(
    MulticlassOptimalBinning("ash"),
    x = w[["ash"]],
    y = w[["target"]],
    algorithm = "optimal",
    prebinning_method = "cart",
    max_n_bins = 6
  )
  b <- transform(m, w[["ash"]], metric = "bins")
  expect_equal(length(b), nrow(w))
  expect_true(is.character(b))

  c <- read.csv(.extdata_path("continuous_synth.csv"))
  mc <- fit(
    ContinuousOptimalBinning("x"),
    x = c[["x"]],
    y = c[["y"]],
    algorithm = "optimal",
    prebinning_method = "cart",
    max_n_bins = 10
  )
  bc <- transform(mc, c[["x"]], metric = "bins")
  expect_equal(length(bc), nrow(c))
  expect_true(is.character(bc))
})
