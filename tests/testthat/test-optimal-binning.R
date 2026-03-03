test_that("fit builds a valid model", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8)
  y <- c(0, 0, 0, 1, 0, 1, 1, 1)

  model <- OptimalBinning("x")
  model <- fit(model, x, y, max_n_bins = 4)

  expect_true(model$fitted)
  expect_true(model$n_bins >= 2)
  expect_true(is.data.frame(model$bin_table))
  expect_true(is.numeric(model$total_iv))
})

test_that("transform returns WOE vector", {
  x <- c(10, 20, 30, 40, 50, 60)
  y <- c(0, 0, 1, 0, 1, 1)

  model <- fit(OptimalBinning("x"), x, y, max_n_bins = 3)
  out <- transform(model, x)

  expect_equal(length(out), length(x))
  expect_true(is.numeric(out))
  expect_false(any(is.na(out)))
})

test_that("optimal algorithm respects bin count and monotonic trend", {
  x <- 1:20
  y <- c(rep(0, 10), rep(1, 10))

  model <- fit(
    OptimalBinning("x"),
    x,
    y,
    algorithm = "optimal",
    max_n_bins = 4,
    max_n_prebins = 8,
    min_bin_size = 3,
    monotonic_trend = "ascending"
  )

  expect_true(model$fitted)
  expect_lte(model$n_bins, 4)
  expect_true(all(model$bin_table$count >= 3))
  expect_true(all(diff(model$bin_table$event_rate) >= -1e-12))
  expect_equal(model$fit_params$algorithm, "optimal")
})

test_that("auto trend selects a feasible monotonic solution", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  y <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0)

  model <- fit(
    OptimalBinning("x"),
    x,
    y,
    algorithm = "optimal",
    max_n_bins = 3,
    max_n_prebins = 6,
    monotonic_trend = "auto"
  )

  expect_true(model$fit_params$monotonic_trend %in% c("ascending", "descending", "none"))
})

test_that("numerical fit supports special and missing bins", {
  x <- c(1, 2, 3, NA, -999, 4, 5, -999, 6, NA)
  y <- c(0, 0, 1, 1, 0, 1, 1, 0, 1, 0)

  model <- fit(
    OptimalBinning("x"),
    x,
    y,
    algorithm = "optimal",
    special_codes = -999
  )

  expect_true("Special" %in% model$bin_table$bin)
  expect_true("Missing" %in% model$bin_table$bin)

  out <- transform(model, x)
  expect_equal(length(out), length(x))
  expect_false(any(is.na(out)))
})

test_that("categorical fit and transform work", {
  x <- c("A", "A", "B", "B", "C", "C", "D", "D")
  y <- c(1, 1, 1, 0, 0, 0, 0, 0)
  model <- fit(
    OptimalBinning("x_cat", dtype = "categorical"),
    x,
    y,
    algorithm = "optimal",
    monotonic_trend = "auto"
  )
  expect_true(model$fitted)
  expect_equal(model$dtype, "categorical")
  expect_equal(model$n_bins, 4)
  out <- transform(model, c("A", "B", "C", "D"))
  expect_equal(length(out), 4)
  expect_true(all(is.finite(out)))
})

test_that("multiclass constructor and fit work", {
  d <- read.csv(.extdata_path("wine_multiclass.csv"), check.names = FALSE)
  m <- fit(
    MulticlassOptimalBinning("ash"),
    d$ash,
    d$target,
    algorithm = "optimal",
    prebinning_method = "cart",
    max_n_bins = 6
  )
  expect_true(m$fitted)
  expect_equal(m$target_dtype, "multiclass")
  expect_equal(length(m$breaks) - 2, 5)
})

test_that("continuous constructor and fit work", {
  d <- read.csv(.extdata_path("continuous_synth.csv"))
  m <- fit(
    ContinuousOptimalBinning("x"),
    d$x,
    d$y,
    algorithm = "optimal",
    prebinning_method = "cart",
    max_n_bins = 10
  )
  expect_true(m$fitted)
  expect_equal(m$target_dtype, "continuous")
  expect_equal(length(m$breaks) - 2, 9)
})

test_that("multiclass and continuous parity references match python", {
  refs <- jsonlite::fromJSON(
    .extdata_path("python_multiclass_continuous_refs.json")
  )

  w <- read.csv(.extdata_path("wine_multiclass.csv"), check.names = FALSE)
  m_mc <- fit(
    MulticlassOptimalBinning("ash"),
    w$ash,
    w$target,
    algorithm = "optimal",
    prebinning_method = "cart",
    max_n_bins = 6,
    max_n_prebins = 20,
    min_bin_size = 0.05,
    monotonic_trend = "auto"
  )
  expect_equal(
    as.numeric(m_mc$breaks[2:(length(m_mc$breaks) - 1)]),
    as.numeric(refs$multiclass_wine_ash$splits),
    tolerance = 1e-6
  )
  expect_equal(m_mc$n_bins, as.integer(refs$multiclass_wine_ash$n_bins))

  c <- read.csv(.extdata_path("continuous_synth.csv"))
  m_ct <- fit(
    ContinuousOptimalBinning("x"),
    c$x,
    c$y,
    algorithm = "optimal",
    prebinning_method = "cart",
    max_n_bins = 10,
    max_n_prebins = 20,
    min_bin_size = 0.05,
    monotonic_trend = "auto"
  )
  expect_equal(
    as.numeric(m_ct$breaks[2:(length(m_ct$breaks) - 1)]),
    as.numeric(refs$continuous_synth_x$splits),
    tolerance = 1e-6
  )
  expect_equal(m_ct$n_bins, as.integer(refs$continuous_synth_x$n_bins))
})

test_that("native solver matches official breast cancer example splits", {
  d <- read.csv(.extdata_path("breast_cancer_mean_radius.csv"))
  model <- fit(
    OptimalBinning("mean radius"),
    d$x,
    d$y,
    algorithm = "optimal",
    solver = "native",
    prebinning_method = "cart",
    max_n_bins = NULL,
    max_n_prebins = 20,
    min_bin_size = 0.05,
    monotonic_trend = "auto"
  )

  expected_splits <- c(11.425000190734863, 12.329999923706055, 13.09499979019165,
                       13.704999923706055, 15.045000076293945, 16.925000190734863)
  model_splits <- model$breaks[2:(length(model$breaks) - 1)]

  expect_equal(length(model_splits), length(expected_splits))
  expect_equal(as.numeric(model_splits), expected_splits, tolerance = 1e-6)
  expect_equal(as.numeric(sum(model$bin_table$iv)), 5.043925466321346, tolerance = 1e-8)
})

test_that("native solver matches multi-case python parity suite", {
  suite_path <- .extdata_path("python_optbinning_reference_suite.json")
  suite <- jsonlite::fromJSON(suite_path, simplifyVector = FALSE)

  for (case in suite$cases) {
    p <- case$params
    data_file <- .extdata_path(case$data_file)
    df <- read.csv(data_file, check.names = FALSE)
    model <- fit(
      OptimalBinning(case$variable, dtype = case$dtype),
      x = df[[case$x_column]],
      y = df[[case$y_column]],
      algorithm = "optimal",
      solver = "native",
      prebinning_method = "cart",
      max_n_bins = if (is.null(p$max_n_bins)) NULL else as.integer(p$max_n_bins),
      max_n_prebins = as.integer(p$max_n_prebins),
      min_bin_size = as.numeric(p$min_prebin_size),
      min_bin_n_event = 1,
      min_bin_n_nonevent = 1,
      monotonic_trend = p$monotonic_trend,
      special_codes = case$special_codes
    )

    if (identical(case$dtype, "numerical")) {
      py_splits <- as.numeric(unlist(case$splits))
      r_splits <- as.numeric(model$breaks[2:(length(model$breaks) - 1)])
      expect_equal(length(r_splits), length(py_splits), info = case$id)
      expect_equal(r_splits, py_splits, tolerance = 1e-4, info = case$id)
    }

    model_core <- subset(model$bin_table, !(bin %in% c("Special", "Missing")))
    expect_equal(nrow(model_core), as.integer(case$n_bins_core), info = case$id)
    expect_equal(sort(as.numeric(model_core$event_rate)), sort(as.numeric(unlist(case$event_rate_core))), tolerance = 1e-8, info = case$id)
    expect_equal(as.numeric(sum(model$bin_table$iv)), as.numeric(case$iv_total), tolerance = 1e-6, info = case$id)
  }
})

test_that("python-cp solver backend fits when optbinning is available", {
  py <- Sys.which("python3")
  if (identical(py, "")) {
    skip("python3 not available")
  }

  status <- suppressWarnings(system2(py, c("-c", "import optbinning"), stdout = FALSE, stderr = FALSE))
  if (!identical(status, 0L)) {
    skip("python package optbinning not available")
  }

  d <- read.csv(.extdata_path("breast_cancer_mean_radius.csv"))
  model <- fit(
    OptimalBinning("mean radius"),
    d$x,
    d$y,
    algorithm = "optimal",
    solver = "python-cp",
    max_n_bins = 7,
    max_n_prebins = 20,
    min_bin_size = 0.05,
    monotonic_trend = "auto"
  )

  expect_true(model$fitted)
  expect_equal(model$fit_params$solver, "python-cp")
  expect_equal(model$n_bins, 7)
  expect_equal(length(model$breaks) - 2, 6)
})

test_that("binning_table fails when model not fitted", {
  model <- OptimalBinning("x")
  expect_error(binning_table(model), "not fitted")
})
