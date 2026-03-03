test_that("localsolver backend fits with engine when available", {
  set.seed(901)
  n <- 500
  x <- rnorm(n)
  y <- rbinom(n, 1, stats::plogis(1.1 * x - 0.4))

  if (!optbinningR:::.python_optbinning_available("ls")) {
    skip("python optbinning LocalSolver backend not available")
  }

  m <- fit(
    OptimalBinning("x"),
    x,
    y,
    algorithm = "optimal",
    solver = "localsolver",
    max_n_bins = 6,
    monotonic_trend = "auto"
  )

  expect_true(m$fitted)
  expect_equal(m$fit_params$solver, "localsolver")
  expect_true(m$n_bins >= 2)
})

test_that("localsolver-native backend fits without python engine", {
  set.seed(903)
  n <- 500
  x <- rnorm(n)
  y <- rbinom(n, 1, stats::plogis(0.9 * x - 0.2))
  m <- fit(
    OptimalBinning("x"),
    x,
    y,
    algorithm = "optimal",
    solver = "localsolver-native",
    max_n_bins = 6
  )
  expect_true(m$fitted)
  expect_equal(m$fit_params$solver, "localsolver-native")
})

test_that("large_scale profile works", {
  set.seed(902)
  n <- 3000
  x <- rnorm(n)
  y <- rbinom(n, 1, stats::plogis(0.8 * x))

  m <- fit(
    OptimalBinning("x"),
    x,
    y,
    algorithm = "optimal",
    solver = "native",
    profile = "large_scale",
    max_n_bins = 8
  )

  expect_true(m$fitted)
  expect_equal(m$fit_params$profile, "large_scale")
  expect_true(m$n_bins >= 2)
})

test_that("fico and telco tutorial workflows run end-to-end", {
  fico <- run_fico_tutorial(n_train = 900, n_update = 400)
  telco <- run_telco_tutorial(n = 1200)

  expect_true(is.finite(fico$train_auc))
  expect_true(is.list(fico$monitoring))
  expect_true(is.finite(fico$monitoring$psi_total))
  expect_true(is.finite(telco$test_auc))
  expect_gt(telco$n_train, 0)
  expect_gt(telco$n_test, 0)
})
