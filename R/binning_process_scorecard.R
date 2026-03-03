#' Create a BinningProcess object
#'
#' @param variable_names Character vector of feature names. If `NULL`, all columns in
#'   the data passed to `fit()` are used.
#' @param target_dtype One of `"binary"`, `"multiclass"` or `"continuous"`.
#' @param binning_fit_params Named list of per-variable fit params. Example:
#'   `list(var1 = list(max_n_bins = 6, monotonic_trend = "auto"))`.
#' @return A `BinningProcess` object.
#' @export
BinningProcess <- function(variable_names = NULL, target_dtype = "binary", binning_fit_params = list()) {
  if (!is.null(variable_names) && (!is.character(variable_names) || anyNA(variable_names))) {
    stop("`variable_names` must be NULL or a character vector without NA.", call. = FALSE)
  }
  if (!target_dtype %in% c("binary", "multiclass", "continuous")) {
    stop("`target_dtype` must be one of 'binary', 'multiclass', 'continuous'.", call. = FALSE)
  }
  if (!is.list(binning_fit_params)) {
    stop("`binning_fit_params` must be a named list.", call. = FALSE)
  }

  structure(
    list(
      variable_names = variable_names,
      target_dtype = target_dtype,
      binning_fit_params = binning_fit_params,
      binning_models = NULL,
      fitted = FALSE
    ),
    class = "BinningProcess"
  )
}

#' @export
fit.BinningProcess <- function(
    object,
    x,
    y,
    max_n_bins = 10L,
    algorithm = "optimal",
    prebinning_method = "cart",
    max_n_prebins = 20L,
    min_bin_size = 0.05,
    min_bin_n_event = 1L,
    min_bin_n_nonevent = 1L,
    monotonic_trend = "none",
    special_codes = NULL,
    solver = "native",
    profile = "standard",
    eps = 1e-6,
    ...
) {
  if (!is.data.frame(x)) {
    stop("For BinningProcess, `x` must be a data.frame.", call. = FALSE)
  }
  if (length(y) != nrow(x)) {
    stop("`x` and `y` must have compatible lengths.", call. = FALSE)
  }

  vars <- object$variable_names
  if (is.null(vars)) {
    vars <- colnames(x)
  }
  if (length(vars) == 0L) {
    stop("No variables selected for BinningProcess.", call. = FALSE)
  }
  missing_vars <- setdiff(vars, colnames(x))
  if (length(missing_vars) > 0L) {
    stop(sprintf("Variables not found in x: %s", paste(missing_vars, collapse = ", ")), call. = FALSE)
  }

  models <- stats::setNames(vector("list", length(vars)), vars)
  for (v in vars) {
    xj <- x[[v]]
    dtype <- if (is.character(xj) || is.factor(xj)) "categorical" else "numerical"
    if (object$target_dtype != "binary" && identical(dtype, "categorical")) {
      stop(sprintf("Categorical predictors are currently only supported for binary target. Variable: %s", v), call. = FALSE)
    }

    model <- switch(
      object$target_dtype,
      binary = OptimalBinning(name = v, dtype = dtype),
      multiclass = MulticlassOptimalBinning(name = v, dtype = "numerical"),
      continuous = ContinuousOptimalBinning(name = v, dtype = "numerical")
    )

    defaults <- list(
      max_n_bins = max_n_bins,
      algorithm = algorithm,
      prebinning_method = prebinning_method,
      max_n_prebins = max_n_prebins,
      min_bin_size = min_bin_size,
      min_bin_n_event = min_bin_n_event,
      min_bin_n_nonevent = min_bin_n_nonevent,
      monotonic_trend = monotonic_trend,
      special_codes = special_codes,
      solver = solver,
      profile = profile,
      eps = eps
    )
    overrides <- object$binning_fit_params[[v]]
    if (is.null(overrides)) {
      overrides <- list()
    }
    args <- c(list(object = model, x = xj, y = y), utils::modifyList(defaults, overrides))
    models[[v]] <- do.call(fit, args)
  }

  object$variable_names <- vars
  object$binning_models <- models
  object$fitted <- TRUE
  object
}

#' @export
transform.BinningProcess <- function(object, x, ...) {
  if (!isTRUE(object$fitted)) {
    stop("BinningProcess is not fitted. Call fit() first.", call. = FALSE)
  }
  if (!is.data.frame(x)) {
    stop("For BinningProcess, `x` must be a data.frame.", call. = FALSE)
  }
  vars <- object$variable_names
  missing_vars <- setdiff(vars, colnames(x))
  if (length(missing_vars) > 0L) {
    stop(sprintf("Variables not found in x: %s", paste(missing_vars, collapse = ", ")), call. = FALSE)
  }

  out <- stats::setNames(vector("list", length(vars)), vars)
  for (v in vars) {
    out[[v]] <- transform(object$binning_models[[v]], x[[v]])
  }
  as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
}

#' Create a Scorecard object
#'
#' @param binning_process A fitted or unfitted `BinningProcess` object with
#'   `target_dtype = "binary"` or `"continuous"`.
#' @param pdo Points to double the odds.
#' @param odds Base odds.
#' @param base_points Base points at the base odds.
#' @return A `Scorecard` object.
#' @export
Scorecard <- function(binning_process, pdo = 20, odds = 50, base_points = 600) {
  if (!inherits(binning_process, "BinningProcess")) {
    stop("`binning_process` must be a BinningProcess object.", call. = FALSE)
  }
  if (!binning_process$target_dtype %in% c("binary", "continuous")) {
    stop("Scorecard currently supports binary or continuous target.", call. = FALSE)
  }
  if (!is.numeric(pdo) || length(pdo) != 1L || pdo <= 0) {
    stop("`pdo` must be a positive numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(odds) || length(odds) != 1L || odds <= 0) {
    stop("`odds` must be a positive numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(base_points) || length(base_points) != 1L) {
    stop("`base_points` must be a numeric scalar.", call. = FALSE)
  }

  structure(
    list(
      binning_process = binning_process,
      pdo = as.numeric(pdo),
      odds = as.numeric(odds),
      base_points = as.numeric(base_points),
      glm_model = NULL,
      points_table = NULL,
      variables = NULL,
      fitted = FALSE
    ),
    class = "Scorecard"
  )
}

#' @export
fit.Scorecard <- function(
    object,
    x,
    y,
    max_n_bins = 10L,
    algorithm = "optimal",
    prebinning_method = "cart",
    max_n_prebins = 20L,
    min_bin_size = 0.05,
    min_bin_n_event = 1L,
    min_bin_n_nonevent = 1L,
    monotonic_trend = "none",
    special_codes = NULL,
    solver = "native",
    profile = "standard",
    eps = 1e-6,
    ...
) {
  if (!is.data.frame(x)) {
    stop("For Scorecard, `x` must be a data.frame.", call. = FALSE)
  }
  y_num <- as.numeric(y)
  if (identical(object$binning_process$target_dtype, "binary") && !all(y_num %in% c(0, 1, NA))) {
    stop("Binary scorecard requires y in {0, 1}.", call. = FALSE)
  }
  if (identical(object$binning_process$target_dtype, "continuous") && any(is.na(y_num))) {
    stop("Continuous scorecard requires numeric y without NA.", call. = FALSE)
  }
  keep <- !is.na(y_num)
  x_fit <- x[keep, , drop = FALSE]
  y_fit <- y_num[keep]

  bp <- object$binning_process
  if (!isTRUE(bp$fitted)) {
    bp <- fit(
      bp, x_fit, y_fit,
      max_n_bins = max_n_bins,
      algorithm = algorithm,
      prebinning_method = prebinning_method,
      max_n_prebins = max_n_prebins,
      min_bin_size = min_bin_size,
      min_bin_n_event = min_bin_n_event,
      min_bin_n_nonevent = min_bin_n_nonevent,
      monotonic_trend = monotonic_trend,
      special_codes = special_codes,
      solver = solver,
      profile = profile,
      eps = eps
    )
  }

  x_woe <- transform(bp, x_fit)
  model_df <- as.data.frame(x_woe, check.names = FALSE)
  model_df$.__y__. <- y_fit
  fitted_model <- if (identical(bp$target_dtype, "binary")) {
    stats::glm(.__y__. ~ ., data = model_df, family = stats::binomial())
  } else {
    stats::lm(.__y__. ~ ., data = model_df)
  }

  coefs <- stats::coef(fitted_model)
  intercept <- unname(coefs["(Intercept)"])
  beta <- coefs[setdiff(names(coefs), "(Intercept)")]
  beta <- beta[!is.na(beta)]

  factor <- object$pdo / log(2)
  offset <- object$base_points + factor * log(object$odds)
  base_intercept_points <- offset - factor * intercept

  pt_rows <- list()
  for (v in names(bp$binning_models)) {
    m <- bp$binning_models[[v]]
    bt <- m$bin_table
    value_col <- if (identical(bp$target_dtype, "binary")) "woe" else "event_rate"
    if (!value_col %in% names(bt)) {
      next
    }
    b <- if (v %in% names(beta)) as.numeric(beta[[v]]) else 0
    vals <- bt[[value_col]]
    pts <- -factor * b * vals
    pt_rows[[v]] <- data.frame(
      variable = rep(v, nrow(bt)),
      bin = bt$bin,
      value = vals,
      coefficient = rep(b, nrow(bt)),
      points = pts,
      stringsAsFactors = FALSE
    )
  }
  points_table <- do.call(rbind, pt_rows)
  if (is.null(points_table)) {
    points_table <- data.frame(
      variable = character(0),
      bin = character(0),
      value = numeric(0),
      coefficient = numeric(0),
      points = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  object$binning_process <- bp
  object$glm_model <- fitted_model
  object$model_type <- if (identical(bp$target_dtype, "binary")) "binomial" else "gaussian"
  object$points_table <- points_table
  object$variables <- names(bp$binning_models)
  object$base_intercept_points <- base_intercept_points
  object$factor <- factor
  object$fitted <- TRUE
  object
}

#' @export
transform.Scorecard <- function(object, x, ...) {
  if (!isTRUE(object$fitted)) {
    stop("Scorecard is not fitted. Call fit() first.", call. = FALSE)
  }
  if (!is.data.frame(x)) {
    stop("For Scorecard, `x` must be a data.frame.", call. = FALSE)
  }
  woe_df <- transform(object$binning_process, x)
  vars <- object$variables
  woe_mat <- as.matrix(woe_df[, vars, drop = FALSE])
  coefs <- stats::coef(object$glm_model)
  beta <- coefs[vars]
  beta[is.na(beta)] <- 0
  linear_pred <- as.numeric(coefs["(Intercept)"] + woe_mat %*% beta)
  object$base_intercept_points - object$factor * (linear_pred - as.numeric(coefs["(Intercept)"]))
}

#' Predict probability from a fitted scorecard
#'
#' @param object A fitted `Scorecard` object.
#' @param x Data frame with predictor columns.
#' @return Numeric vector of event probabilities.
#' @export
predict_proba <- function(object, x) {
  if (!inherits(object, "Scorecard")) {
    stop("`object` must be a Scorecard instance.", call. = FALSE)
  }
  if (!isTRUE(object$fitted)) {
    stop("Scorecard is not fitted. Call fit() first.", call. = FALSE)
  }
  if (!identical(object$model_type, "binomial")) {
    stop("`predict_proba()` is only available for binary scorecards.", call. = FALSE)
  }
  woe_df <- transform(object$binning_process, x)
  vars <- object$variables
  woe_mat <- as.matrix(woe_df[, vars, drop = FALSE])
  coefs <- stats::coef(object$glm_model)
  beta <- coefs[vars]
  beta[is.na(beta)] <- 0
  logit <- as.numeric(coefs["(Intercept)"] + woe_mat %*% beta)
  stats::plogis(logit)
}

#' Predict target from a fitted scorecard model
#'
#' @param object A fitted `Scorecard` object.
#' @param x Data frame with predictor columns.
#' @return Numeric predictions on model scale (`[0, 1]` for binary as probabilities,
#'   continuous values for continuous target).
#' @export
predict_score <- function(object, x) {
  if (!inherits(object, "Scorecard")) {
    stop("`object` must be a Scorecard instance.", call. = FALSE)
  }
  if (!isTRUE(object$fitted)) {
    stop("Scorecard is not fitted. Call fit() first.", call. = FALSE)
  }
  if (identical(object$model_type, "binomial")) {
    return(predict_proba(object, x))
  }
  woe_df <- transform(object$binning_process, x)
  vars <- object$variables
  woe_mat <- as.matrix(woe_df[, vars, drop = FALSE])
  coefs <- stats::coef(object$glm_model)
  beta <- coefs[vars]
  beta[is.na(beta)] <- 0
  as.numeric(coefs["(Intercept)"] + woe_mat %*% beta)
}

.score_auc_binary <- function(y, p) {
  y <- as.numeric(y)
  p <- as.numeric(p)
  keep <- !(is.na(y) | is.na(p))
  y <- y[keep]
  p <- p[keep]
  if (length(y) == 0L || !all(y %in% c(0, 1))) {
    return(NA_real_)
  }
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  if (n1 == 0L || n0 == 0L) {
    return(NA_real_)
  }
  r <- rank(p, ties.method = "average")
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

#' Scorecard monitoring metrics (PSI and optional performance drift)
#'
#' @param object A fitted `Scorecard` object.
#' @param x_ref Reference population predictors.
#' @param x_new New/current population predictors.
#' @param y_ref Optional reference target.
#' @param y_new Optional new/current target.
#' @param n_bins Number of score buckets for PSI.
#' @return A list with PSI table and summary metrics.
#' @export
scorecard_monitoring <- function(object, x_ref, x_new, y_ref = NULL, y_new = NULL, n_bins = 10L) {
  if (!inherits(object, "Scorecard") || !isTRUE(object$fitted)) {
    stop("`object` must be a fitted Scorecard.", call. = FALSE)
  }
  if (!is.data.frame(x_ref) || !is.data.frame(x_new)) {
    stop("`x_ref` and `x_new` must be data.frames.", call. = FALSE)
  }
  if (!is.numeric(n_bins) || length(n_bins) != 1L || n_bins < 2L) {
    stop("`n_bins` must be numeric scalar >= 2.", call. = FALSE)
  }
  n_bins <- as.integer(n_bins)

  s_ref <- transform(object, x_ref)
  s_new <- transform(object, x_new)
  probs <- seq(0, 1, length.out = n_bins + 1L)
  cuts <- unique(stats::quantile(s_ref, probs = probs, na.rm = TRUE, names = FALSE, type = 8))
  if (length(cuts) < (n_bins + 1L)) {
    rng <- range(s_ref, na.rm = TRUE)
    if (is.finite(rng[1]) && is.finite(rng[2]) && rng[1] < rng[2]) {
      cuts <- seq(rng[1], rng[2], length.out = n_bins + 1L)
    }
  }
  if (length(unique(cuts)) < 2L) {
    stop("Cannot compute PSI: reference scores have insufficient variability.", call. = FALSE)
  }
  if (cuts[1] > -Inf) {
    cuts[1] <- -Inf
  }
  if (cuts[length(cuts)] < Inf) {
    cuts[length(cuts)] <- Inf
  }

  b_ref <- cut(s_ref, breaks = cuts, include.lowest = TRUE, right = TRUE)
  b_new <- cut(s_new, breaks = cuts, include.lowest = TRUE, right = TRUE)
  cnt_ref <- as.numeric(table(b_ref))
  cnt_new <- as.numeric(table(b_new))
  pct_ref <- cnt_ref / sum(cnt_ref)
  pct_new <- cnt_new / sum(cnt_new)
  eps <- 1e-8
  psi_i <- (pct_new - pct_ref) * log((pct_new + eps) / (pct_ref + eps))

  psi_table <- data.frame(
    bin = levels(b_ref),
    count_ref = cnt_ref,
    count_new = cnt_new,
    pct_ref = pct_ref,
    pct_new = pct_new,
    psi = psi_i,
    stringsAsFactors = FALSE
  )

  auc_ref <- NA_real_
  auc_new <- NA_real_
  if (identical(object$model_type, "binomial")) {
    p_ref <- predict_proba(object, x_ref)
    p_new <- predict_proba(object, x_new)
    if (!is.null(y_ref)) {
      auc_ref <- .score_auc_binary(y_ref, p_ref)
    }
    if (!is.null(y_new)) {
      auc_new <- .score_auc_binary(y_new, p_new)
    }
  }

  list(
    psi_total = sum(psi_i, na.rm = TRUE),
    psi_table = psi_table,
    auc_ref = auc_ref,
    auc_new = auc_new,
    auc_delta = auc_new - auc_ref
  )
}

.scorecard_linear_predict <- function(object, values_named) {
  coefs <- stats::coef(object$glm_model)
  vars <- object$variables
  beta <- coefs[vars]
  beta[is.na(beta)] <- 0
  as.numeric(coefs["(Intercept)"] + sum(beta * values_named[vars]))
}

.scorecard_model_predict <- function(object, values_named) {
  lp <- .scorecard_linear_predict(object, values_named)
  if (identical(object$model_type, "binomial")) stats::plogis(lp) else lp
}

#' Counterfactual explanation for a fitted scorecard
#'
#' @param object A fitted `Scorecard` object.
#' @param x_one One-row data.frame with predictor values.
#' @param target Desired prediction target (probability for binary, value for continuous).
#' @param direction One of `"auto"`, `">="` or `"<="`.
#' @param max_changes Maximum number of variables allowed to change.
#' @param max_combinations Maximum combinations evaluated for search.
#' @return A list with base prediction, counterfactual prediction and variable changes.
#' @export
counterfactual_scorecard <- function(
    object,
    x_one,
    target,
    direction = "auto",
    max_changes = 2L,
    max_combinations = 50000L
) {
  if (!inherits(object, "Scorecard") || !isTRUE(object$fitted)) {
    stop("`object` must be a fitted Scorecard.", call. = FALSE)
  }
  if (!is.data.frame(x_one) || nrow(x_one) != 1L) {
    stop("`x_one` must be a one-row data.frame.", call. = FALSE)
  }
  if (!is.numeric(target) || length(target) != 1L || is.na(target)) {
    stop("`target` must be a non-missing numeric scalar.", call. = FALSE)
  }
  if (!direction %in% c("auto", ">=", "<=")) {
    stop("`direction` must be one of 'auto', '>=', '<='.", call. = FALSE)
  }
  if (!is.numeric(max_changes) || length(max_changes) != 1L || max_changes < 0) {
    stop("`max_changes` must be a numeric scalar >= 0.", call. = FALSE)
  }
  if (!is.numeric(max_combinations) || length(max_combinations) != 1L || max_combinations < 1) {
    stop("`max_combinations` must be a numeric scalar >= 1.", call. = FALSE)
  }

  vars <- object$variables
  base_vals <- as.numeric(transform(object$binning_process, x_one)[1, vars, drop = TRUE])
  names(base_vals) <- vars
  base_pred <- .scorecard_model_predict(object, base_vals)

  if (identical(object$model_type, "binomial") && (target < 0 || target > 1)) {
    stop("For binary scorecard, `target` must be in [0, 1].", call. = FALSE)
  }

  if (identical(direction, "auto")) {
    direction <- if (target >= base_pred) ">=" else "<="
  }
  is_feasible <- function(pred) {
    if (identical(direction, ">=")) pred >= target else pred <= target
  }

  cand_map <- split(object$points_table[, c("variable", "bin", "value")], object$points_table$variable)
  candidates <- lapply(vars, function(v) {
    tab <- cand_map[[v]]
    if (is.null(tab)) {
      data.frame(bin = NA_character_, value = base_vals[[v]], stringsAsFactors = FALSE)
    } else {
      out <- unique(tab[, c("bin", "value"), drop = FALSE])
      out <- out[is.finite(out$value), , drop = FALSE]
      base_row <- data.frame(bin = "__base__", value = base_vals[[v]], stringsAsFactors = FALSE)
      unique(rbind(base_row, out))
    }
  })
  names(candidates) <- vars

  best <- list(
    found = is_feasible(base_pred),
    pred = base_pred,
    vals = base_vals,
    changed = character(0),
    dist = 0,
    gap = abs(base_pred - target)
  )

  eval_count <- 0L
  consider <- function(vals_new, changed_vars) {
    pred_new <- .scorecard_model_predict(object, vals_new)
    gap_new <- abs(pred_new - target)
    dist_new <- sum(abs(vals_new[changed_vars] - base_vals[changed_vars]))
    feasible_new <- is_feasible(pred_new)
    better <- FALSE
    if (feasible_new && !best$found) {
      better <- TRUE
    } else if (feasible_new && best$found) {
      if (length(changed_vars) < length(best$changed) ||
          (length(changed_vars) == length(best$changed) && dist_new < best$dist) ||
          (length(changed_vars) == length(best$changed) && abs(dist_new - best$dist) < 1e-12 && gap_new < best$gap)) {
        better <- TRUE
      }
    } else if (!best$found && gap_new < best$gap) {
      better <- TRUE
    }
    if (better) {
      best$found <<- feasible_new
      best$pred <<- pred_new
      best$vals <<- vals_new
      best$changed <<- changed_vars
      best$dist <<- dist_new
      best$gap <<- gap_new
    }
  }

  for (k in 0:min(as.integer(max_changes), length(vars))) {
    if (k == 0L) {
      eval_count <- eval_count + 1L
      consider(base_vals, character(0))
      next
    }
    subsets <- utils::combn(vars, k, simplify = FALSE)
    for (sv in subsets) {
      opts <- lapply(sv, function(v) candidates[[v]][, c("bin", "value"), drop = FALSE])
      names(opts) <- sv
      idx_grid <- expand.grid(lapply(opts, function(df) seq_len(nrow(df))), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      for (ri in seq_len(nrow(idx_grid))) {
        vals_new <- base_vals
        for (j in seq_along(sv)) {
          v <- sv[j]
          vals_new[[v]] <- opts[[v]]$value[idx_grid[ri, j]]
        }
        eval_count <- eval_count + 1L
        consider(vals_new, sv)
        if (eval_count >= as.integer(max_combinations)) {
          break
        }
      }
      if (eval_count >= as.integer(max_combinations)) {
        break
      }
    }
    if (eval_count >= as.integer(max_combinations)) {
      break
    }
    if (best$found) {
      break
    }
  }

  get_bin_for_value <- function(v, val) {
    tab <- candidates[[v]]
    if (is.null(tab) || nrow(tab) == 0L) return(NA_character_)
    idx <- which(abs(tab$value - val) < 1e-12)
    if (length(idx) == 0L) return(NA_character_)
    as.character(tab$bin[idx[1]])
  }
  changed <- best$changed
  changes_df <- data.frame(
    variable = changed,
    from_value = as.numeric(base_vals[changed]),
    to_value = as.numeric(best$vals[changed]),
    from_bin = vapply(changed, function(v) get_bin_for_value(v, base_vals[[v]]), character(1)),
    to_bin = vapply(changed, function(v) get_bin_for_value(v, best$vals[[v]]), character(1)),
    stringsAsFactors = FALSE
  )

  list(
    found = best$found,
    base_prediction = base_pred,
    counterfactual_prediction = best$pred,
    target = target,
    direction = direction,
    n_changed = length(changed),
    changed_variables = changed,
    changes = changes_df,
    evaluated_combinations = eval_count
  )
}

#' Refit an existing BinningProcess on new data
#'
#' @param object A `BinningProcess` object.
#' @param x Data frame of predictors.
#' @param y Target vector.
#' @param variables Optional subset of variables to update. If `NULL`, all
#'   process variables are refit.
#' @param ... Additional arguments forwarded to `fit.BinningProcess()`.
#' @return Updated `BinningProcess` object.
#' @export
update_binning_process <- function(object, x, y, variables = NULL, ...) {
  if (!inherits(object, "BinningProcess")) {
    stop("`object` must be a BinningProcess instance.", call. = FALSE)
  }
  if (!is.data.frame(x)) {
    stop("`x` must be a data.frame.", call. = FALSE)
  }
  vars <- object$variable_names
  if (is.null(vars)) {
    vars <- colnames(x)
  }
  if (!is.null(variables)) {
    if (!is.character(variables) || anyNA(variables)) {
      stop("`variables` must be a character vector without NA.", call. = FALSE)
    }
    bad <- setdiff(variables, vars)
    if (length(bad) > 0L) {
      stop(sprintf("Variables not in process: %s", paste(bad, collapse = ", ")), call. = FALSE)
    }
    vars <- variables
  }

  tmp <- object
  tmp$variable_names <- vars
  updated <- do.call(fit, c(list(object = tmp, x = x, y = y), list(...)))
  object$binning_models[vars] <- updated$binning_models[vars]
  object$fitted <- TRUE
  object
}

#' @export
print.BinningProcess <- function(x, ...) {
  cat("BinningProcess\n")
  cat("  Target: ", x$target_dtype, "\n", sep = "")
  cat("  Fitted: ", x$fitted, "\n", sep = "")
  if (isTRUE(x$fitted)) {
    cat("  Variables: ", paste(x$variable_names, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

#' @export
print.Scorecard <- function(x, ...) {
  cat("Scorecard\n")
  cat("  Fitted: ", x$fitted, "\n", sep = "")
  if (isTRUE(x$fitted)) {
    cat("  Model: ", x$model_type, "\n", sep = "")
    cat("  Variables: ", paste(x$variables, collapse = ", "), "\n", sep = "")
    cat("  Points table rows: ", nrow(x$points_table), "\n", sep = "")
  }
  invisible(x)
}
