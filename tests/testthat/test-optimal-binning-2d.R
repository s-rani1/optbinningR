test_that("binary OptimalBinning2D fits and transforms", {
  set.seed(202)
  n <- 500
  x1 <- rnorm(n)
  x2 <- runif(n, -2, 2)
  p <- plogis(0.9 * x1 - 0.6 * x2)
  y <- rbinom(n, 1, p)
  x <- data.frame(x1 = x1, x2 = x2)

  ob2 <- OptimalBinning2D("x1", "x2", target_dtype = "binary")
  ob2 <- fit(ob2, x, y, max_n_bins_x = 6, max_n_bins_y = 5, min_bin_size = 0.01)

  expect_true(ob2$fitted)
  expect_true(ob2$n_bins > 0)
  bt <- build(binning_table(ob2))
  expect_true(all(c("bin", "count", "event", "non_event", "event_rate", "woe", "iv", "ix", "iy") %in% names(bt)))
  out <- transform(ob2, x)
  expect_equal(length(out), n)
  expect_true(any(is.finite(out)))
})

test_that("continuous OptimalBinning2D fits and transforms", {
  set.seed(203)
  n <- 450
  x1 <- rnorm(n)
  x2 <- rnorm(n, 0.5)
  y <- 2 + 1.2 * x1 - 0.7 * x2 + rnorm(n, sd = 0.3)
  x <- data.frame(x1 = x1, x2 = x2)

  ob2 <- OptimalBinning2D("x1", "x2", target_dtype = "continuous")
  ob2 <- fit(ob2, x, y, max_n_bins_x = 5, max_n_bins_y = 5, min_bin_size = 0.01)

  expect_true(ob2$fitted)
  bt <- build(binning_table(ob2))
  expect_true(all(is.na(bt$woe)))
  out <- transform(ob2, x)
  expect_equal(length(out), n)
  expect_true(any(is.finite(out)))
})
