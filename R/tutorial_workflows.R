#' Run a native FICO-style end-to-end tutorial workflow
#'
#' @param n_train Number of training samples.
#' @param n_update Number of update-period samples.
#' @return A list with fitted artifacts and summary metrics.
#' @export
run_fico_tutorial <- function(
    n_train = 2500L,
    n_update = 1200L,
    train_data = NULL,
    y_train = NULL,
    update_data = NULL,
    y_update = NULL,
    solver = "native",
    profile = "standard",
    prebinning_method = "quantile",
    max_n_bins = 6L,
    max_n_prebins = 20L
) {
  set.seed(1201)
  solver_choice <- solver
  n_train <- as.integer(n_train)
  n_update <- as.integer(n_update)
  if (n_train < 200L || n_update < 100L) {
    stop("`n_train` and `n_update` are too small.", call. = FALSE)
  }

  if (is.null(train_data) || is.null(y_train)) {
    d_train <- data.frame(
      external_risk_estimate = round(stats::runif(n_train, 20, 95)),
      revolving_utilization = stats::runif(n_train, 0, 1),
      age_oldest_trade = stats::rexp(n_train, rate = 1 / 120),
      delinquency_30 = stats::rpois(n_train, lambda = 0.25),
      inquiry_6m = stats::rpois(n_train, lambda = 1.4)
    )
    logit <- -3.2 +
      0.035 * (100 - d_train$external_risk_estimate) +
      1.9 * d_train$revolving_utilization +
      0.2 * d_train$delinquency_30 +
      0.08 * d_train$inquiry_6m -
      0.003 * d_train$age_oldest_trade
    y_train <- stats::rbinom(n_train, 1, stats::plogis(logit))
  } else {
    d_train <- as.data.frame(train_data)
    y_train <- as.numeric(y_train)
    if (nrow(d_train) != length(y_train)) stop("train_data and y_train size mismatch.", call. = FALSE)
  }

  vars <- names(d_train)
  bp <- BinningProcess(variable_names = vars, target_dtype = "binary")
  bp <- fit(
    bp, d_train, y_train,
    algorithm = "optimal",
    solver = solver_choice,
    profile = profile,
    prebinning_method = prebinning_method,
    max_n_bins = max_n_bins,
    max_n_prebins = max_n_prebins
  )

  sc <- Scorecard(bp, pdo = 20, odds = 50, base_points = 600)
  sc <- fit(
    sc, d_train, y_train,
    algorithm = "optimal",
    solver = solver_choice,
    profile = profile,
    prebinning_method = prebinning_method,
    max_n_bins = max_n_bins,
    max_n_prebins = max_n_prebins
  )
  p_train <- predict_proba(sc, d_train)
  auc_train <- .score_auc_binary(y_train, p_train)

  if (is.null(update_data) || is.null(y_update)) {
    d_upd <- data.frame(
      external_risk_estimate = round(stats::runif(n_update, 18, 92)),
      revolving_utilization = pmin(1, pmax(0, stats::runif(n_update, 0, 1) + stats::rnorm(n_update, 0.04, 0.08))),
      age_oldest_trade = stats::rexp(n_update, rate = 1 / 110),
      delinquency_30 = stats::rpois(n_update, lambda = 0.35),
      inquiry_6m = stats::rpois(n_update, lambda = 1.6)
    )
    logit_u <- -3.1 +
      0.038 * (100 - d_upd$external_risk_estimate) +
      1.95 * d_upd$revolving_utilization +
      0.23 * d_upd$delinquency_30 +
      0.09 * d_upd$inquiry_6m -
      0.0025 * d_upd$age_oldest_trade
    y_upd <- stats::rbinom(n_update, 1, stats::plogis(logit_u))
  } else {
    d_upd <- as.data.frame(update_data)
    y_upd <- as.numeric(y_update)
    if (nrow(d_upd) != length(y_upd)) stop("update_data and y_update size mismatch.", call. = FALSE)
  }

  # Updating binning process as in tutorial.
  bp_upd <- update_binning_process(
    bp, d_upd, y_upd,
    algorithm = "optimal",
    solver = solver_choice,
    profile = profile,
    prebinning_method = prebinning_method,
    max_n_bins = max_n_bins,
    max_n_prebins = max_n_prebins
  )
  sc_upd <- Scorecard(bp_upd, pdo = 20, odds = 50, base_points = 600)
  sc_upd <- fit(
    sc_upd, d_upd, y_upd,
    algorithm = "optimal",
    solver = solver_choice,
    profile = profile,
    prebinning_method = prebinning_method,
    max_n_bins = max_n_bins,
    max_n_prebins = max_n_prebins
  )

  mon <- scorecard_monitoring(sc, d_train, d_upd, y_ref = y_train, y_new = y_upd, n_bins = 10)
  list(
    train_auc = auc_train,
    monitoring = mon,
    n_train = n_train,
    n_update = n_update,
    model = sc,
    updated_model = sc_upd
  )
}

#' Run a native Telco churn end-to-end tutorial workflow
#'
#' @param n Number of samples.
#' @return A list with fitted artifacts and summary metrics.
#' @export
run_telco_tutorial <- function(
    n = 3000L,
    train_data = NULL,
    y_train = NULL,
    test_data = NULL,
    y_test = NULL,
    solver = "native",
    profile = "standard",
    prebinning_method = "quantile",
    max_n_bins = 6L,
    max_n_prebins = 20L
) {
  set.seed(1307)
  solver_choice <- solver
  n <- as.integer(n)
  if (n < 300L) {
    stop("`n` is too small.", call. = FALSE)
  }

  if (is.null(train_data) || is.null(y_train) || is.null(test_data) || is.null(y_test)) {
    contract <- sample(c("Month-to-month", "One year", "Two year"), n, replace = TRUE, prob = c(0.55, 0.25, 0.20))
    internet <- sample(c("DSL", "Fiber optic", "No"), n, replace = TRUE, prob = c(0.35, 0.45, 0.20))
    tenure <- pmax(1, round(stats::rgamma(n, shape = 2.4, scale = 12)))
    monthly_charges <- pmax(18, stats::rnorm(n, mean = 70, sd = 25))
    total_charges <- pmax(20, tenure * monthly_charges + stats::rnorm(n, sd = 60))
    payment_method <- sample(c("Electronic check", "Mailed check", "Bank transfer", "Credit card"), n, replace = TRUE)

    d <- data.frame(
      tenure = tenure,
      monthly_charges = monthly_charges,
      total_charges = total_charges,
      contract = contract,
      internet_service = internet,
      payment_method = payment_method,
      stringsAsFactors = FALSE
    )

    logit <- -2.0 +
      0.95 * (contract == "Month-to-month") +
      0.45 * (internet == "Fiber optic") +
      0.40 * (payment_method == "Electronic check") +
      0.012 * (monthly_charges - 70) -
      0.03 * pmin(tenure, 48)
    y <- stats::rbinom(n, 1, stats::plogis(logit))

    idx <- sample.int(n, size = floor(0.75 * n))
    d_train <- d[idx, , drop = FALSE]
    y_train <- y[idx]
    d_test <- d[-idx, , drop = FALSE]
    y_test <- y[-idx]
  } else {
    d_train <- as.data.frame(train_data)
    d_test <- as.data.frame(test_data)
    y_train <- as.numeric(y_train)
    y_test <- as.numeric(y_test)
    if (nrow(d_train) != length(y_train)) stop("train_data and y_train size mismatch.", call. = FALSE)
    if (nrow(d_test) != length(y_test)) stop("test_data and y_test size mismatch.", call. = FALSE)
  }

  bp <- BinningProcess(variable_names = names(d_train), target_dtype = "binary")
  bp <- fit(
    bp, d_train, y_train,
    algorithm = "optimal",
    solver = solver_choice,
    profile = profile,
    prebinning_method = prebinning_method,
    max_n_bins = max_n_bins,
    max_n_prebins = max_n_prebins
  )
  sc <- fit(
    Scorecard(bp), d_train, y_train,
    algorithm = "optimal",
    solver = solver_choice,
    profile = profile,
    prebinning_method = prebinning_method,
    max_n_bins = max_n_bins,
    max_n_prebins = max_n_prebins
  )
  p_test <- predict_proba(sc, d_test)
  auc_test <- .score_auc_binary(y_test, p_test)

  list(
    test_auc = auc_test,
    n_train = nrow(d_train),
    n_test = nrow(d_test),
    model = sc
  )
}
