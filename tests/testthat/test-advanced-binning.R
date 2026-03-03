test_that("OptimalPWBinning works for binary and continuous targets", {
  set.seed(501)
  n <- 400
  x <- rnorm(n)
  yb <- rbinom(n, 1, plogis(1.1 * x - 0.2))
  yc <- 1.5 + 0.8 * x + rnorm(n, sd = 0.25)

  m_bin <- fit(
    OptimalPWBinning("x", target_dtype = "binary", degree = 2),
    x,
    yb,
    algorithm = "optimal",
    max_n_bins = 6
  )
  pb <- transform(m_bin, x)
  expect_true(m_bin$fitted)
  expect_equal(length(pb), n)
  expect_true(all(pb >= 0 & pb <= 1, na.rm = TRUE))

  m_ct <- fit(
    OptimalPWBinning("x", target_dtype = "continuous", degree = 2),
    x,
    yc,
    algorithm = "optimal",
    max_n_bins = 6
  )
  pc <- transform(m_ct, x)
  expect_true(m_ct$fitted)
  expect_equal(length(pc), n)
  expect_true(any(is.finite(pc)))
})

test_that("OptimalBinningSketch supports streaming partial_fit and fit", {
  set.seed(502)
  n <- 1500
  x <- rnorm(n)
  y <- rbinom(n, 1, plogis(0.9 * x))

  sk <- OptimalBinningSketch("x", sample_size = 400)
  sk <- partial_fit(sk, x[1:700], y[1:700])
  sk <- partial_fit(sk, x[701:1500], y[701:1500])
  sk <- fit(sk, algorithm = "optimal", max_n_bins = 6)
  tr <- transform(sk, x)

  expect_true(sk$fitted)
  expect_true(sk$n_seen == n)
  expect_equal(length(sk$x_sample), 400)
  expect_equal(length(tr), n)
  expect_true(any(is.finite(tr)))
})

test_that("OptimalBinningUncertainty fits and transforms", {
  set.seed(503)
  n <- 500
  x <- rnorm(n)
  p <- plogis(0.8 * x - 0.1)
  y <- rbinom(n, 1, p)
  lo <- pmax(0, y - runif(n, 0, 0.3))
  hi <- pmin(1, y + runif(n, 0, 0.3))
  u <- runif(n, 0, 1)

  m <- fit(
    OptimalBinningUncertainty("x", uncertainty_strategy = "pessimistic"),
    x,
    y_lower = lo,
    y_upper = hi,
    uncertainty = u,
    algorithm = "optimal",
    max_n_bins = 6
  )
  tr <- transform(m, x)
  expect_true(m$fitted)
  expect_equal(length(tr), n)
  expect_true(any(is.finite(tr)))
})
