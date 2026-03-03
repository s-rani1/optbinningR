test_that("binning process fits and transforms multiple variables", {
  set.seed(42)
  n <- 200
  x1 <- rnorm(n)
  x2 <- runif(n)
  p <- plogis(1.2 * x1 - 0.8 * x2)
  y <- rbinom(n, 1, p)
  df <- data.frame(x1 = x1, x2 = x2)

  bp <- BinningProcess(variable_names = c("x1", "x2"), target_dtype = "binary")
  bp <- fit(
    bp,
    df,
    y,
    algorithm = "optimal",
    max_n_bins = 6,
    monotonic_trend = "auto"
  )

  expect_true(bp$fitted)
  expect_equal(sort(names(bp$binning_models)), c("x1", "x2"))

  tr <- transform(bp, df)
  expect_equal(ncol(tr), 2)
  expect_equal(nrow(tr), n)
  expect_true(all(is.finite(as.matrix(tr))))
})

test_that("scorecard fits and returns points and probabilities", {
  set.seed(123)
  n <- 300
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  p <- plogis(0.7 * x1 - 1.1 * x2)
  y <- rbinom(n, 1, p)
  df <- data.frame(x1 = x1, x2 = x2)

  bp <- BinningProcess(variable_names = c("x1", "x2"), target_dtype = "binary")
  sc <- Scorecard(bp, pdo = 20, odds = 50, base_points = 600)
  sc <- fit(
    sc,
    df,
    y,
    algorithm = "optimal",
    max_n_bins = 6,
    monotonic_trend = "auto"
  )

  expect_true(sc$fitted)
  expect_true(inherits(sc$glm_model, "glm"))
  expect_true(nrow(sc$points_table) > 0)
  expect_true(all(c("variable", "bin", "value", "coefficient", "points") %in% names(sc$points_table)))

  pts <- transform(sc, df)
  pr <- predict_proba(sc, df)
  expect_equal(length(pts), n)
  expect_equal(length(pr), n)
  expect_true(all(is.finite(pts)))
  expect_true(all(pr >= 0 & pr <= 1))
})

test_that("continuous scorecard fits and predicts", {
  set.seed(8)
  n <- 280
  x1 <- rnorm(n)
  x2 <- runif(n, -2, 2)
  y <- 3 + 1.1 * x1 - 0.6 * x2 + rnorm(n, sd = 0.3)
  df <- data.frame(x1 = x1, x2 = x2)

  bp <- BinningProcess(variable_names = c("x1", "x2"), target_dtype = "continuous")
  sc <- Scorecard(bp, pdo = 20, odds = 50, base_points = 600)
  sc <- fit(sc, df, y, algorithm = "optimal", max_n_bins = 6, monotonic_trend = "auto")

  expect_true(sc$fitted)
  expect_equal(sc$model_type, "gaussian")
  pred <- predict_score(sc, df)
  pts <- transform(sc, df)
  expect_equal(length(pred), n)
  expect_equal(length(pts), n)
  expect_true(all(is.finite(pred)))
  expect_true(all(is.finite(pts)))
})

test_that("scorecard monitoring computes psi and auc drift", {
  set.seed(77)
  n <- 300
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  p <- plogis(0.8 * x1 - 0.5 * x2)
  y <- rbinom(n, 1, p)
  df_ref <- data.frame(x1 = x1, x2 = x2)

  x1_new <- rnorm(n, mean = 0.4, sd = 1.2)
  x2_new <- rnorm(n, mean = -0.2, sd = 1.0)
  p_new <- plogis(0.8 * x1_new - 0.5 * x2_new)
  y_new <- rbinom(n, 1, p_new)
  df_new <- data.frame(x1 = x1_new, x2 = x2_new)

  bp <- BinningProcess(variable_names = c("x1", "x2"), target_dtype = "binary")
  sc <- fit(Scorecard(bp), df_ref, y, algorithm = "optimal", max_n_bins = 6)
  mon <- scorecard_monitoring(sc, df_ref, df_new, y_ref = y, y_new = y_new, n_bins = 10)

  expect_true(is.list(mon))
  expect_true(mon$psi_total >= 0)
  expect_equal(nrow(mon$psi_table), 10)
  expect_true(is.finite(mon$auc_ref))
  expect_true(is.finite(mon$auc_new))
})

test_that("update_binning_process updates subset variables", {
  set.seed(101)
  n <- 240
  d1 <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = runif(n)
  )
  y1 <- rbinom(n, 1, plogis(0.9 * d1$x1 - 0.4 * d1$x2))

  bp <- fit(BinningProcess(c("x1", "x2", "x3"), target_dtype = "binary"), d1, y1, algorithm = "optimal", max_n_bins = 6)
  before <- bp$binning_models$x1$breaks

  d2 <- data.frame(
    x1 = rnorm(n, mean = 1),
    x2 = rnorm(n),
    x3 = runif(n)
  )
  y2 <- rbinom(n, 1, plogis(0.9 * d2$x1 - 0.4 * d2$x2))
  bp2 <- update_binning_process(bp, d2, y2, variables = "x1", algorithm = "optimal", max_n_bins = 6)
  after <- bp2$binning_models$x1$breaks

  expect_true(bp2$fitted)
  expect_false(isTRUE(all.equal(before, after)))
})

test_that("counterfactual_scorecard works for binary scorecard", {
  set.seed(333)
  n <- 260
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- rbinom(n, 1, plogis(0.9 * x1 - 0.7 * x2))
  df <- data.frame(x1 = x1, x2 = x2)

  sc <- fit(Scorecard(BinningProcess(c("x1", "x2"), "binary")), df, y, algorithm = "optimal", max_n_bins = 6)
  x0 <- df[1, , drop = FALSE]
  p0 <- predict_proba(sc, x0)
  target <- min(0.99, p0 + 0.10)
  cf <- counterfactual_scorecard(sc, x0, target = target, max_changes = 2)

  expect_true(is.list(cf))
  expect_true(cf$n_changed <= 2)
  expect_true(cf$counterfactual_prediction >= p0 - 1e-12)
  if (isTRUE(cf$found)) {
    expect_true(cf$counterfactual_prediction >= target - 1e-12)
  } else {
    expect_true(abs(cf$counterfactual_prediction - target) <= abs(p0 - target) + 1e-12)
  }
})

test_that("counterfactual_scorecard works for continuous scorecard", {
  set.seed(444)
  n <- 260
  x1 <- rnorm(n)
  x2 <- runif(n, -2, 2)
  y <- 2.5 + 0.9 * x1 - 0.5 * x2 + rnorm(n, sd = 0.4)
  df <- data.frame(x1 = x1, x2 = x2)

  sc <- fit(Scorecard(BinningProcess(c("x1", "x2"), "continuous")), df, y, algorithm = "optimal", max_n_bins = 6)
  x0 <- df[1, , drop = FALSE]
  y0 <- predict_score(sc, x0)
  target <- y0 + 0.3
  cf <- counterfactual_scorecard(sc, x0, target = target, max_changes = 2)

  expect_true(is.list(cf))
  expect_true(cf$n_changed <= 2)
  expect_true(cf$counterfactual_prediction >= y0 - 1e-12)
  if (isTRUE(cf$found)) {
    expect_true(cf$counterfactual_prediction >= target - 1e-12)
  } else {
    expect_true(abs(cf$counterfactual_prediction - target) <= abs(y0 - target) + 1e-12)
  }
})
