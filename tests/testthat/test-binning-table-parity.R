test_that("binary build returns python-style columns and totals row", {
  d <- read.csv(.extdata_path("breast_cancer_mean_radius.csv"))
  m <- fit(
    OptimalBinning("mean radius"),
    d$x,
    d$y,
    algorithm = "optimal",
    solver = "native",
    prebinning_method = "cart",
    max_n_prebins = 20,
    min_bin_size = 0.05,
    monotonic_trend = "auto"
  )

  bt <- build(binning_table(m))
  expect_true(all(c("Bin", "Count", "Count (%)", "Non-event", "Event", "Event rate", "WoE", "IV", "JS") %in% names(bt)))
  expect_equal(rownames(bt)[length(rownames(bt))], "Totals")
  expect_equal(as.numeric(bt["Totals", "Count (%)"]), 1, tolerance = 1e-12)
})

test_that("continuous build returns tutorial-style statistic columns", {
  d <- read.csv(.extdata_path("continuous_synth.csv"))
  m <- fit(
    ContinuousOptimalBinning("x"),
    d$x,
    d$y,
    algorithm = "optimal",
    prebinning_method = "cart",
    max_n_bins = 10
  )

  bt <- build(binning_table(m))
  expect_true(all(c("Bin", "Count", "Count (%)", "Sum", "Std", "Mean", "Min", "Max", "Zeros count", "WoE", "IV") %in% names(bt)))
  expect_equal(rownames(bt)[length(rownames(bt))], "Totals")
  expect_equal(as.numeric(bt["Totals", "Count (%)"]), 1, tolerance = 1e-12)
})

test_that("multiclass analysis on BinningTable returns expected sections", {
  w <- read.csv(.extdata_path("wine_multiclass.csv"), check.names = FALSE)
  m <- fit(
    MulticlassOptimalBinning("ash"),
    w$ash,
    w$target,
    algorithm = "optimal",
    prebinning_method = "cart",
    max_n_bins = 6
  )

  bto <- binning_table(m)
  out <- capture.output(an <- analysis(bto))
  expect_true(any(grepl("Multiclass Binning Table Analysis", out, fixed = TRUE)))
  expect_true(is.list(an))
  expect_true(all(c("js", "hhi", "hhi_norm", "cramers_v", "quality_score", "monotonic_trend", "significance_tests") %in% names(an)))
})
