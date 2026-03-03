#' Create an OptimalBinning model object
#'
#' @param name Feature name.
#' @param dtype Feature type: `"numerical"` or `"categorical"`.
#' @return An `OptimalBinning` object.
#' @export
OptimalBinning <- function(name, dtype = "numerical") {
  if (!is.character(name) || length(name) != 1L || nchar(name) == 0L) {
    stop("`name` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!dtype %in% c("numerical", "categorical")) {
    stop("`dtype` must be 'numerical' or 'categorical'.", call. = FALSE)
  }

  structure(
    list(
      name = name,
      dtype = dtype,
      target_dtype = "binary",
      fitted = FALSE,
      breaks = NULL,
      bin_table = NULL,
      n_bins = NULL,
      total_iv = NULL,
      fit_params = NULL,
      special_codes = NULL,
      category_woe = NULL,
      classes = NULL,
      status = NULL,
      splits = NULL,
      user_splits = NULL,
      user_splits_fixed = NULL,
      fit_data = NULL
    ),
    class = "OptimalBinning"
  )
}

#' Create a MulticlassOptimalBinning model object
#'
#' @param name Feature name.
#' @param dtype Feature type. Currently only `"numerical"` is supported.
#' @return A `MulticlassOptimalBinning` object.
#' @export
MulticlassOptimalBinning <- function(name, dtype = "numerical") {
  if (!is.character(name) || length(name) != 1L || nchar(name) == 0L) {
    stop("`name` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!identical(dtype, "numerical")) {
    stop("Currently only dtype = 'numerical' is supported for multiclass.", call. = FALSE)
  }

  structure(
    list(
      name = name,
      dtype = dtype,
      target_dtype = "multiclass",
      fitted = FALSE,
      breaks = NULL,
      bin_table = NULL,
      n_bins = NULL,
      total_iv = NULL,
      fit_params = NULL,
      special_codes = NULL,
      category_woe = NULL,
      classes = NULL,
      status = NULL,
      splits = NULL,
      user_splits = NULL,
      user_splits_fixed = NULL,
      fit_data = NULL
    ),
    class = c("MulticlassOptimalBinning", "OptimalBinning")
  )
}

#' Create a ContinuousOptimalBinning model object
#'
#' @param name Feature name.
#' @param dtype Feature type. Currently only `"numerical"` is supported.
#' @return A `ContinuousOptimalBinning` object.
#' @export
ContinuousOptimalBinning <- function(name, dtype = "numerical") {
  if (!is.character(name) || length(name) != 1L || nchar(name) == 0L) {
    stop("`name` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!identical(dtype, "numerical")) {
    stop("Currently only dtype = 'numerical' is supported for continuous target.", call. = FALSE)
  }

  structure(
    list(
      name = name,
      dtype = dtype,
      target_dtype = "continuous",
      fitted = FALSE,
      breaks = NULL,
      bin_table = NULL,
      n_bins = NULL,
      total_iv = NULL,
      fit_params = NULL,
      special_codes = NULL,
      category_woe = NULL,
      classes = NULL,
      status = NULL,
      splits = NULL,
      user_splits = NULL,
      user_splits_fixed = NULL,
      fit_data = NULL
    ),
    class = c("ContinuousOptimalBinning", "OptimalBinning")
  )
}

#' Fit an OptimalBinning model
#'
#' @param object An `OptimalBinning` object.
#' @param x Numerical predictor vector.
#' @param y Binary target vector (0/1).
#' @param max_n_bins Maximum final bins. Set `NULL` for no explicit upper bound in optimal mode.
#' @param algorithm Binning algorithm: `"quantile"` or `"optimal"`.
#' @param prebinning_method Prebinning method for optimal mode: `"cart"` or `"quantile"`.
#' @param max_n_prebins Maximum pre-bins used by the `"optimal"` algorithm.
#' @param min_bin_size Minimum bin size as integer count or fraction in `(0, 1]`.
#' @param min_bin_n_event Minimum number of events required per bin (native solver).
#' @param min_bin_n_nonevent Minimum number of non-events required per bin (native solver).
#' @param monotonic_trend One of `"none"`, `"ascending"`, `"descending"`, or `"auto"`.
#' @param special_codes Optional vector of values treated as a dedicated `Special` bin.
#' @param solver Solver backend for `algorithm = "optimal"`:
#'   `"native"`, `"localsolver"` (actual LocalSolver engine via Python),
#'   `"localsolver-native"` (native LocalSolver-compatible path) or `"python-cp"`.
#' @param profile Optimization profile. Use `"large_scale"` for large datasets.
#' @param eps Small constant for safe WOE computation.
#' @return Fitted `OptimalBinning` object.
#' @export
fit <- function(
    object,
    ...
) {
  UseMethod("fit")
}

#' @export
fit.OptimalBinning <- function(
    object,
    x,
    y,
    max_n_bins = 10L,
    algorithm = "quantile",
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
    min_event_rate_diff = 0,
    max_pvalue = NULL,
    max_pvalue_policy = "consecutive",
    user_splits = NULL,
    user_splits_fixed = NULL,
    verbose = FALSE,
    ...
) {
  .validate_fit_inputs(
    x, y, object$dtype, object$target_dtype, max_n_bins, algorithm, max_n_prebins, min_bin_size,
    min_bin_n_event, min_bin_n_nonevent, monotonic_trend, solver, prebinning_method, profile,
    min_event_rate_diff, max_pvalue, max_pvalue_policy, user_splits, user_splits_fixed, verbose
  )

  if (identical(object$target_dtype, "continuous")) {
    y_num <- as.numeric(y)
  } else {
    y_num <- as.numeric(y)
  }
  keep_y <- !is.na(y_num)
  x <- x[keep_y]
  y_num <- y_num[keep_y]

  is_missing <- is.na(x)
  is_special <- .is_special(x, special_codes)
  is_clean <- !(is_missing | is_special)

  min_bin_count <- .resolve_min_bin_count(min_bin_size, sum(is_clean))
  max_n_bins_int <- if (is.null(max_n_bins)) NA_integer_ else as.integer(max_n_bins)

  if (sum(is_clean) < 2L) {
    stop("Not enough clean observations after removing missing/special values.", call. = FALSE)
  }

  if (identical(object$target_dtype, "continuous")) {
    fit_out <- .fit_continuous(
      x = x[is_clean],
      y = y_num[is_clean],
      max_n_bins = max_n_bins_int,
      prebinning_method = prebinning_method,
      max_n_prebins = as.integer(max_n_prebins),
      min_bin_count = min_bin_count,
      monotonic_trend = monotonic_trend
    )
  } else if (identical(object$target_dtype, "multiclass")) {
    solver_mc <- if (solver %in% c("cp", "mip", "native")) solver else "native"
    fit_out <- .fit_multiclass(
      x = x[is_clean],
      y = y_num[is_clean],
      max_n_bins = max_n_bins_int,
      prebinning_method = prebinning_method,
      max_n_prebins = as.integer(max_n_prebins),
      min_bin_count = min_bin_count,
      monotonic_trend = monotonic_trend,
      min_event_rate_diff = as.numeric(min_event_rate_diff),
      max_pvalue = if (is.null(max_pvalue)) NULL else as.numeric(max_pvalue),
      max_pvalue_policy = max_pvalue_policy,
      user_splits = user_splits,
      user_splits_fixed = user_splits_fixed,
      verbose = isTRUE(verbose),
      solver = solver_mc
    )
  } else if (identical(object$dtype, "categorical")) {
    total_event <- sum(y_num == 1, na.rm = TRUE)
    total_non_event <- sum(y_num == 0, na.rm = TRUE)
    if (total_event == 0 || total_non_event == 0) {
      stop("`y` must include both classes 0 and 1.", call. = FALSE)
    }
    fit_out <- .fit_categorical(
      x = x[is_clean],
      y = y_num[is_clean],
      eps = eps
    )
  } else if (identical(algorithm, "quantile")) {
    total_event <- sum(y_num == 1, na.rm = TRUE)
    total_non_event <- sum(y_num == 0, na.rm = TRUE)
    if (total_event == 0 || total_non_event == 0) {
      stop("`y` must include both classes 0 and 1.", call. = FALSE)
    }
    fit_out <- .fit_quantile(x[is_clean], y_num[is_clean], max_n_bins_int, eps)
  } else if (identical(solver, "python-cp")) {
    total_event <- sum(y_num == 1, na.rm = TRUE)
    total_non_event <- sum(y_num == 0, na.rm = TRUE)
    if (total_event == 0 || total_non_event == 0) {
      stop("`y` must include both classes 0 and 1.", call. = FALSE)
    }
    fit_out <- .fit_optimal_python_cp(
      x = x[is_clean],
      y = y_num[is_clean],
      name = object$name,
      max_n_bins = max_n_bins_int,
      prebinning_method = prebinning_method,
      max_n_prebins = as.integer(max_n_prebins),
      min_bin_count = min_bin_count,
      min_bin_n_event = as.integer(min_bin_n_event),
      min_bin_n_nonevent = as.integer(min_bin_n_nonevent),
      monotonic_trend = monotonic_trend,
      eps = eps
    )
  } else if (identical(solver, "localsolver")) {
    total_event <- sum(y_num == 1, na.rm = TRUE)
    total_non_event <- sum(y_num == 0, na.rm = TRUE)
    if (total_event == 0 || total_non_event == 0) {
      stop("`y` must include both classes 0 and 1.", call. = FALSE)
    }
    fit_out <- .fit_optimal_python_ls(
      x = x[is_clean],
      y = y_num[is_clean],
      name = object$name,
      max_n_bins = max_n_bins_int,
      prebinning_method = prebinning_method,
      max_n_prebins = as.integer(max_n_prebins),
      min_bin_count = min_bin_count,
      min_bin_n_event = as.integer(min_bin_n_event),
      min_bin_n_nonevent = as.integer(min_bin_n_nonevent),
      monotonic_trend = monotonic_trend,
      eps = eps
    )
  } else {
    total_event <- sum(y_num == 1, na.rm = TRUE)
    total_non_event <- sum(y_num == 0, na.rm = TRUE)
    if (total_event == 0 || total_non_event == 0) {
      stop("`y` must include both classes 0 and 1.", call. = FALSE)
    }
    if (identical(profile, "large_scale")) {
      fit_out <- .fit_optimal_large_scale(
        x = x[is_clean],
        y = y_num[is_clean],
        max_n_bins = max_n_bins_int,
        max_n_prebins = as.integer(max_n_prebins),
        min_bin_count = min_bin_count,
        min_bin_n_event = as.integer(min_bin_n_event),
        min_bin_n_nonevent = as.integer(min_bin_n_nonevent),
        monotonic_trend = monotonic_trend,
        eps = eps
      )
    } else if (identical(solver, "localsolver-native")) {
      fit_out <- .fit_optimal_localsolver(
        x = x[is_clean],
        y = y_num[is_clean],
        max_n_bins = max_n_bins_int,
        prebinning_method = prebinning_method,
        max_n_prebins = as.integer(max_n_prebins),
        min_bin_count = min_bin_count,
        min_bin_n_event = as.integer(min_bin_n_event),
        min_bin_n_nonevent = as.integer(min_bin_n_nonevent),
        monotonic_trend = monotonic_trend,
        eps = eps
      )
    } else {
      fit_out <- .fit_optimal(
        x = x[is_clean],
        y = y_num[is_clean],
        max_n_bins = max_n_bins_int,
        prebinning_method = prebinning_method,
        max_n_prebins = as.integer(max_n_prebins),
        min_bin_count = min_bin_count,
        min_bin_n_event = as.integer(min_bin_n_event),
        min_bin_n_nonevent = as.integer(min_bin_n_nonevent),
        monotonic_trend = monotonic_trend,
        eps = eps
      )
    }
  }

  if (identical(object$target_dtype, "binary")) {
    fit_out$bin_table <- .append_special_missing_bins(
      bin_table = fit_out$bin_table,
      y_all = y_num,
      is_special = is_special,
      is_missing = is_missing,
      eps = eps
    )
  }

  object$fitted <- TRUE
  object$breaks <- fit_out$breaks
  object$bin_table <- fit_out$bin_table
  object$n_bins <- nrow(fit_out$bin_table)
  object$total_iv <- if ("iv" %in% names(fit_out$bin_table)) sum(fit_out$bin_table$iv, na.rm = TRUE) else NA_real_
  object$special_codes <- special_codes
  if (identical(object$dtype, "categorical")) {
    object$category_woe <- stats::setNames(fit_out$bin_table$woe, fit_out$bin_table$bin)
  } else {
    object$category_woe <- NULL
  }
  object$fit_params <- list(
    algorithm = algorithm,
    prebinning_method = prebinning_method,
    max_n_bins = max_n_bins_int,
    max_n_prebins = as.integer(max_n_prebins),
    min_bin_count = min_bin_count,
    min_bin_n_event = as.integer(min_bin_n_event),
    min_bin_n_nonevent = as.integer(min_bin_n_nonevent),
    monotonic_trend = fit_out$monotonic_trend,
    special_codes = special_codes,
    solver = solver,
    profile = profile
  )
  object$classes <- if (!is.null(fit_out$classes)) fit_out$classes else NULL
  object$status <- if (!is.null(fit_out$status)) fit_out$status else "OPTIMAL"
  if (!is.null(fit_out$breaks) && length(fit_out$breaks) > 2L) {
    object$splits <- as.numeric(fit_out$breaks[2:(length(fit_out$breaks) - 1L)])
  } else {
    object$splits <- numeric(0)
  }
  object$user_splits <- user_splits
  object$user_splits_fixed <- user_splits_fixed
  object$fit_data <- list(
    x = x,
    y = y_num,
    is_special = is_special,
    is_missing = is_missing
  )

  object
}

.validate_fit_inputs <- function(
    x, y, dtype, target_dtype, max_n_bins, algorithm, max_n_prebins, min_bin_size,
    min_bin_n_event, min_bin_n_nonevent, monotonic_trend, solver, prebinning_method, profile,
    min_event_rate_diff, max_pvalue, max_pvalue_policy, user_splits, user_splits_fixed, verbose
) {
  if (identical(dtype, "numerical")) {
    if (!is.numeric(x)) {
      stop("`x` must be numeric when dtype = 'numerical'.", call. = FALSE)
    }
  } else {
    if (!(is.character(x) || is.factor(x))) {
      stop("`x` must be character/factor when dtype = 'categorical'.", call. = FALSE)
    }
  }
  if (length(x) != length(y)) {
    stop("`x` and `y` must have the same length.", call. = FALSE)
  }

  y_num <- suppressWarnings(as.numeric(y))
  if (identical(target_dtype, "binary")) {
    if (!all(y_num %in% c(0, 1, NA))) {
      stop("`y` must be binary with values in {0, 1}.", call. = FALSE)
    }
  } else if (identical(target_dtype, "multiclass")) {
    if (any(is.na(y_num))) {
      stop("`y` must be numeric/integer-like for multiclass.", call. = FALSE)
    }
    if (length(unique(y_num)) < 3L) {
      stop("`y` must have at least 3 classes for multiclass.", call. = FALSE)
    }
  } else if (identical(target_dtype, "continuous")) {
    if (!is.numeric(y_num) || any(is.na(y_num))) {
      stop("`y` must be numeric for continuous target.", call. = FALSE)
    }
  }

  if (!(is.null(max_n_bins) || (is.numeric(max_n_bins) && length(max_n_bins) == 1L && max_n_bins >= 2L))) {
    stop("`max_n_bins` must be NULL or a numeric scalar >= 2.", call. = FALSE)
  }

  if (!is.character(algorithm) || length(algorithm) != 1L || !algorithm %in% c("quantile", "optimal")) {
    stop("`algorithm` must be one of 'quantile' or 'optimal'.", call. = FALSE)
  }
  if (identical(algorithm, "quantile") && is.null(max_n_bins)) {
    stop("`max_n_bins` cannot be NULL when algorithm = 'quantile'.", call. = FALSE)
  }
  if (identical(dtype, "categorical") && !identical(algorithm, "optimal")) {
    stop("For dtype = 'categorical', only algorithm = 'optimal' is currently supported.", call. = FALSE)
  }
  if (!target_dtype %in% c("binary", "multiclass", "continuous")) {
    stop("Invalid target type.", call. = FALSE)
  }

  if (!is.numeric(max_n_prebins) || length(max_n_prebins) != 1L || max_n_prebins < 2L) {
    stop("`max_n_prebins` must be a numeric scalar >= 2.", call. = FALSE)
  }

  if (!is.numeric(min_bin_size) || length(min_bin_size) != 1L || min_bin_size <= 0) {
    stop("`min_bin_size` must be a positive numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(min_bin_n_event) || length(min_bin_n_event) != 1L || min_bin_n_event < 0) {
    stop("`min_bin_n_event` must be a non-negative numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(min_bin_n_nonevent) || length(min_bin_n_nonevent) != 1L || min_bin_n_nonevent < 0) {
    stop("`min_bin_n_nonevent` must be a non-negative numeric scalar.", call. = FALSE)
  }

  allowed_trend <- c("none", "ascending", "descending", "peak", "valley", "auto", "auto_heuristic", "auto_asc_desc")
  if (identical(target_dtype, "multiclass")) {
    trend_ok <- (is.character(monotonic_trend) && length(monotonic_trend) == 1L && monotonic_trend %in% allowed_trend) ||
      (is.list(monotonic_trend) && length(monotonic_trend) >= 1L)
    if (!trend_ok) {
      stop("For multiclass, `monotonic_trend` must be a supported scalar or a per-class list.", call. = FALSE)
    }
  } else {
    if (!is.character(monotonic_trend) || length(monotonic_trend) != 1L || !monotonic_trend %in% allowed_trend) {
      stop("`monotonic_trend` must be one of 'none', 'ascending', 'descending', 'auto'.", call. = FALSE)
    }
  }

  allowed_solver <- if (identical(target_dtype, "multiclass")) {
    c("native", "cp", "mip")
  } else {
    c("native", "localsolver", "localsolver-native", "python-cp")
  }
  if (!is.character(solver) || length(solver) != 1L || !solver %in% allowed_solver) {
    stop("`solver` has an unsupported value for this target type.", call. = FALSE)
  }

  allowed_prebin <- c("cart", "quantile")
  if (!is.character(prebinning_method) || length(prebinning_method) != 1L || !prebinning_method %in% allowed_prebin) {
    stop("`prebinning_method` must be one of 'cart' or 'quantile'.", call. = FALSE)
  }

  allowed_profile <- c("standard", "large_scale")
  if (!is.character(profile) || length(profile) != 1L || !profile %in% allowed_profile) {
    stop("`profile` must be one of 'standard' or 'large_scale'.", call. = FALSE)
  }
  if (!is.numeric(min_event_rate_diff) || length(min_event_rate_diff) != 1L || is.na(min_event_rate_diff) || min_event_rate_diff < 0) {
    stop("`min_event_rate_diff` must be a non-negative numeric scalar.", call. = FALSE)
  }
  if (!is.null(max_pvalue) && (!is.numeric(max_pvalue) || length(max_pvalue) != 1L || is.na(max_pvalue) || max_pvalue <= 0 || max_pvalue >= 1)) {
    stop("`max_pvalue` must be NULL or a numeric scalar in (0,1).", call. = FALSE)
  }
  if (!is.character(max_pvalue_policy) || length(max_pvalue_policy) != 1L || !max_pvalue_policy %in% c("consecutive", "all")) {
    stop("`max_pvalue_policy` must be 'consecutive' or 'all'.", call. = FALSE)
  }
  if (!is.null(user_splits) && !is.numeric(user_splits)) {
    stop("`user_splits` must be NULL or numeric.", call. = FALSE)
  }
  if (!is.null(user_splits_fixed) && !is.logical(user_splits_fixed)) {
    stop("`user_splits_fixed` must be NULL or logical.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("`verbose` must be TRUE/FALSE.", call. = FALSE)
  }
}

.resolve_min_bin_count <- function(min_bin_size, n) {
  if (min_bin_size <= 1) {
    return(max(1L, as.integer(ceiling(min_bin_size * n))))
  }
  as.integer(ceiling(min_bin_size))
}

.fit_quantile <- function(x, y_num, max_n_bins, eps) {
  probs <- seq(0, 1, length.out = as.integer(max_n_bins) + 1L)
  q <- as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7))
  breaks <- unique(q)
  if (length(breaks) < 2L) {
    breaks <- c(min(x), max(x))
  }

  breaks <- .expand_boundaries(breaks)
  bins <- cut(x, breaks = breaks, include.lowest = TRUE, right = TRUE)

  bin_table <- .bin_table_from_factor_bins(bins, y_num, eps)
  list(breaks = breaks, bin_table = bin_table, monotonic_trend = "none")
}

.fit_categorical <- function(x, y, eps) {
  x_chr <- as.character(x)
  cats <- sort(unique(x_chr))
  event <- numeric(length(cats))
  non_event <- numeric(length(cats))

  for (i in seq_along(cats)) {
    mask <- x_chr == cats[i]
    event[i] <- sum(y[mask] == 1)
    non_event[i] <- sum(y[mask] == 0)
  }

  bin_table <- .recompute_woe_from_counts(
    data.frame(
      bin = cats,
      count = event + non_event,
      event = event,
      non_event = non_event,
      stringsAsFactors = FALSE
    ),
    eps = eps
  )

  list(breaks = NULL, bin_table = bin_table, monotonic_trend = "none")
}

.fit_multiclass <- function(
    x, y, max_n_bins, prebinning_method, max_n_prebins, min_bin_count, monotonic_trend,
    min_event_rate_diff = 0, max_pvalue = NULL, max_pvalue_policy = "consecutive",
    user_splits = NULL, user_splits_fixed = NULL, verbose = FALSE, solver = "native"
) {
  classes <- sort(unique(y))
  if (length(classes) < 3L) {
    stop("Multiclass requires at least 3 classes.", call. = FALSE)
  }

  if (!is.null(user_splits) && length(user_splits) > 0L) {
    us <- sort(unique(as.numeric(user_splits)))
    us <- us[is.finite(us)]
    pre_breaks <- unique(c(min(x), us, max(x)))
    # user_splits_fixed present means keep user-defined prebins untouched.
    if (!isTRUE(any(user_splits_fixed))) {
      if (length(pre_breaks) > 2L) {
        refined <- .refine_splits_remove_pure_multiclass(pre_breaks[2:(length(pre_breaks) - 1L)], x, y)
        pre_breaks <- unique(c(min(x), refined, max(x)))
      }
    }
  } else {
    pre_breaks <- .build_prebreaks(
      x = x,
      y = y,
      mode = "multiclass",
      method = prebinning_method,
      max_n_prebins = max_n_prebins,
      min_bin_count = min_bin_count
    )
    if (length(pre_breaks) > 2L) {
      refined <- .refine_splits_remove_pure_multiclass(pre_breaks[2:(length(pre_breaks) - 1L)], x, y)
      pre_breaks <- unique(c(min(x), refined, max(x)))
    }
  }
  pre_breaks <- .expand_boundaries(unique(pre_breaks))
  pre_bins <- cut(x, breaks = pre_breaks, include.lowest = TRUE, right = TRUE)

  lv <- levels(pre_bins)
  m <- length(lv)
  class_counts <- matrix(0, nrow = m, ncol = length(classes))
  bin_count <- numeric(m)
  for (i in seq_len(m)) {
    mask <- pre_bins == lv[i]
    bin_count[i] <- sum(mask)
    for (k in seq_along(classes)) {
      class_counts[i, k] <- sum(y[mask] == classes[k])
    }
  }

  keep <- bin_count > 0
  class_counts <- class_counts[keep, , drop = FALSE]
  bin_count <- bin_count[keep]
  m <- length(bin_count)
  if (m < 1L) {
    stop("No non-empty pre-bins for multiclass.", call. = FALSE)
  }

  total_counts <- colSums(class_counts)
  total_n <- sum(total_counts)

  cum_counts <- apply(class_counts, 2, cumsum)
  cum_counts <- rbind(rep(0, ncol(cum_counts)), cum_counts)
  cum_n <- c(0, cumsum(bin_count))
  max_bins_eff <- if (is.na(max_n_bins)) m else min(max_n_bins, m)
  prebin_rates <- class_counts / as.vector(bin_count)
  if (is.list(monotonic_trend)) {
    class_trends <- vapply(seq_along(classes), function(k) {
      if (k <= length(monotonic_trend) && !is.null(monotonic_trend[[k]])) as.character(monotonic_trend[[k]]) else "none"
    }, character(1))
  } else if (monotonic_trend %in% c("auto", "auto_heuristic", "auto_asc_desc")) {
    class_trends <- vapply(seq_along(classes), function(k) {
      .auto_monotonic_from_values(bin_count, prebin_rates[, k], monotonic_trend)
    }, character(1))
  } else {
    class_trends <- rep(monotonic_trend, length(classes))
  }
  class_trends[is.na(class_trends) | class_trends == "NULL"] <- "none"

  segment_stats <- function(i, j) {
    seg_class <- cum_counts[j + 1L, ] - cum_counts[i, ]
    seg_n <- cum_n[j + 1L] - cum_n[i]
    if (seg_n <= 0) {
      return(list(score = -Inf, count = 0, probs = rep(NA_real_, length(classes))))
    }
    probs <- seg_class / seg_n
    eps <- 1e-12
    score <- 0
    for (k in seq_along(seg_class)) {
      event <- seg_class[k]
      non_event <- seg_n - event
      total_event <- total_counts[k]
      total_non_event <- total_n - total_event
      p <- max(event / total_event, eps)
      q <- max(non_event / total_non_event, eps)
      score <- score + (p - q) * log(p / q)
    }
    list(score = score, count = seg_n, probs = probs, class_counts = as.numeric(seg_class))
  }

  eval_partition <- function(ends) {
    starts <- c(1L, utils::head(ends + 1L, -1L))
    stats_list <- vector("list", length(ends))
    for (k in seq_along(ends)) {
      s <- segment_stats(starts[k], ends[k])
      if (s$count < min_bin_count) {
        return(NULL)
      }
      if (any(s$class_counts <= 0)) {
        return(NULL)
      }
      stats_list[[k]] <- s
    }
    probs_mat <- t(vapply(stats_list, function(z) z$probs, numeric(length(classes))))
    for (k in seq_along(classes)) {
      if (!.check_monotonic(probs_mat[, k], class_trends[k])) {
        return(NULL)
      }
    }
    if (is.finite(min_event_rate_diff) && min_event_rate_diff > 0 && nrow(probs_mat) > 1L) {
      for (k in seq_len(ncol(probs_mat))) {
        if (any(abs(diff(probs_mat[, k])) < min_event_rate_diff)) {
          return(NULL)
        }
      }
    }
    if (!is.null(max_pvalue) && nrow(probs_mat) > 1L) {
      counts_mat <- t(vapply(stats_list, function(z) z$class_counts, numeric(length(classes))))
      pair_ok <- function(a, b) {
        tab <- rbind(counts_mat[a, ], counts_mat[b, ])
        pval <- tryCatch(suppressWarnings(stats::chisq.test(tab)$p.value), error = function(e) 1)
        !is.na(pval) && pval <= max_pvalue
      }
      if (identical(max_pvalue_policy, "consecutive")) {
        for (i in seq_len(nrow(counts_mat) - 1L)) {
          if (!pair_ok(i, i + 1L)) return(NULL)
        }
      } else {
        for (i in seq_len(nrow(counts_mat) - 1L)) {
          for (j in seq.int(i + 1L, nrow(counts_mat))) {
            if (!pair_ok(i, j)) return(NULL)
          }
        }
      }
    }
    list(ends = ends, stats = stats_list, objective = sum(vapply(stats_list, function(z) z$score, numeric(1))))
  }

  best <- NULL
  recurse <- function(start_idx, bins_used, ends) {
    if (start_idx > m) {
      if (bins_used >= 1L && bins_used <= max_bins_eff) {
        cand <- eval_partition(ends)
        if (!is.null(cand) && (is.null(best) || cand$objective > best$objective)) {
          best <<- cand
        }
      }
      return(invisible(NULL))
    }
    if (bins_used >= max_bins_eff) {
      return(invisible(NULL))
    }
    for (end_idx in seq.int(start_idx, m)) {
      recurse(end_idx + 1L, bins_used + 1L, c(ends, end_idx))
    }
  }
  recurse(1L, 0L, integer(0))
  if (is.null(best)) {
    stop("No feasible multiclass partition.", call. = FALSE)
  }
  if (isTRUE(verbose)) {
    message(sprintf("[multiclass] solver=%s bins=%d objective=%.6f", solver, length(best$ends), best$objective))
  }

  starts <- c(1L, utils::head(best$ends + 1L, -1L))
  final_breaks <- numeric(length(best$ends) + 1L)
  final_breaks[1] <- pre_breaks[1]
  labels <- character(length(best$ends))
  for (k in seq_along(best$ends)) {
    lo <- pre_breaks[starts[k]]
    hi <- pre_breaks[best$ends[k] + 1L]
    labels[k] <- .format_interval_label(lo, hi, include_lowest = (k == 1L))
    final_breaks[k + 1L] <- hi
  }
  final_breaks <- .expand_boundaries(final_breaks)

  event_rate <- vapply(best$stats, function(z) max(z$probs), numeric(1))
  class_counts_mat <- do.call(rbind, lapply(best$stats, function(z) z$class_counts))
  class_rate_mat <- do.call(rbind, lapply(best$stats, function(z) z$probs))
  class_count_names <- paste0("event_class_", make.names(classes))
  class_rate_names <- paste0("event_rate_class_", make.names(classes))
  # Keep compatibility with existing table schema using placeholder columns.
  bin_table <- data.frame(
    bin = labels,
    count = vapply(best$stats, function(z) z$count, numeric(1)),
    event = NA_real_,
    non_event = NA_real_,
    event_rate = event_rate,
    woe = NA_real_,
    iv = vapply(best$stats, function(z) z$score, numeric(1)),
    stringsAsFactors = FALSE
  )
  for (j in seq_along(classes)) {
    bin_table[[class_count_names[j]]] <- as.numeric(class_counts_mat[, j])
    bin_table[[class_rate_names[j]]] <- as.numeric(class_rate_mat[, j])
  }
  list(
    breaks = final_breaks,
    bin_table = bin_table,
    monotonic_trend = "none",
    classes = classes,
    status = "OPTIMAL"
  )
}

.fit_continuous <- function(x, y, max_n_bins, prebinning_method, max_n_prebins, min_bin_count, monotonic_trend) {
  pre_breaks <- .build_prebreaks(
    x = x,
    y = y,
    mode = "continuous",
    method = prebinning_method,
    max_n_prebins = max_n_prebins,
    min_bin_count = min_bin_count
  )
  pre_breaks <- .expand_boundaries(unique(pre_breaks))
  pre_bins <- cut(x, breaks = pre_breaks, include.lowest = TRUE, right = TRUE)
  lv <- levels(pre_bins)
  m <- length(lv)

  sum_y <- numeric(m)
  sum_y2 <- numeric(m)
  count <- numeric(m)
  for (i in seq_len(m)) {
    mask <- pre_bins == lv[i]
    count[i] <- sum(mask)
    sum_y[i] <- sum(y[mask])
    sum_y2[i] <- sum(y[mask]^2)
  }
  keep <- count > 0
  sum_y <- sum_y[keep]
  sum_y2 <- sum_y2[keep]
  count <- count[keep]
  m <- length(count)
  if (m < 1L) {
    stop("No non-empty pre-bins for continuous target.", call. = FALSE)
  }

  cum_y <- c(0, cumsum(sum_y))
  cum_y2 <- c(0, cumsum(sum_y2))
  cum_n <- c(0, cumsum(count))
  total_mean <- sum(sum_y) / sum(count)
  max_bins_eff <- if (is.na(max_n_bins)) m else min(max_n_bins, m)
  prebin_mean <- sum_y / count
  trend_use <- monotonic_trend
  if (monotonic_trend %in% c("auto", "auto_heuristic", "auto_asc_desc")) {
    trend_use <- .auto_monotonic_from_values(count, prebin_mean, monotonic_trend)
  }

  segment_stats <- function(i, j) {
    n <- cum_n[j + 1L] - cum_n[i]
    sy <- cum_y[j + 1L] - cum_y[i]
    sy2 <- cum_y2[j + 1L] - cum_y2[i]
    mean_y <- sy / n
    score <- abs(mean_y - total_mean)
    list(count = n, sse = sy2 - (sy * sy) / n, mean = mean_y, score = score)
  }

  eval_partition <- function(ends) {
    starts <- c(1L, utils::head(ends + 1L, -1L))
    stats_list <- vector("list", length(ends))
    for (k in seq_along(ends)) {
      s <- segment_stats(starts[k], ends[k])
      if (s$count < min_bin_count) {
        return(NULL)
      }
      stats_list[[k]] <- s
    }
    means <- vapply(stats_list, function(z) z$mean, numeric(1))
    if (!.check_monotonic(means, trend_use)) {
      return(NULL)
    }
    list(ends = ends, stats = stats_list, objective = sum(vapply(stats_list, function(z) z$score, numeric(1))))
  }

  best <- NULL
  recurse <- function(start_idx, bins_used, ends) {
    if (start_idx > m) {
      if (bins_used >= 1L && bins_used <= max_bins_eff) {
        cand <- eval_partition(ends)
        if (!is.null(cand) && (is.null(best) || cand$objective > best$objective)) {
          best <<- cand
        }
      }
      return(invisible(NULL))
    }
    if (bins_used >= max_bins_eff) {
      return(invisible(NULL))
    }
    for (end_idx in seq.int(start_idx, m)) {
      recurse(end_idx + 1L, bins_used + 1L, c(ends, end_idx))
    }
  }
  recurse(1L, 0L, integer(0))
  if (is.null(best)) {
    stop("No feasible continuous partition.", call. = FALSE)
  }

  starts <- c(1L, utils::head(best$ends + 1L, -1L))
  final_breaks <- numeric(length(best$ends) + 1L)
  final_breaks[1] <- pre_breaks[1]
  labels <- character(length(best$ends))
  for (k in seq_along(best$ends)) {
    lo <- pre_breaks[starts[k]]
    hi <- pre_breaks[best$ends[k] + 1L]
    labels[k] <- .format_interval_label(lo, hi, include_lowest = (k == 1L))
    final_breaks[k + 1L] <- hi
  }
  final_breaks <- .expand_boundaries(final_breaks)

  bin_table <- data.frame(
    bin = labels,
    count = vapply(best$stats, function(z) z$count, numeric(1)),
    event = NA_real_,
    non_event = NA_real_,
    event_rate = vapply(best$stats, function(z) z$mean, numeric(1)),
    woe = NA_real_,
    iv = vapply(best$stats, function(z) z$score, numeric(1)),
    stringsAsFactors = FALSE
  )
  list(breaks = final_breaks, bin_table = bin_table, monotonic_trend = "none")
}

.is_special <- function(x, special_codes) {
  if (is.null(special_codes) || length(special_codes) == 0L) {
    return(rep(FALSE, length(x)))
  }
  x_chr <- as.character(x)
  special_chr <- as.character(special_codes)
  !is.na(x_chr) & x_chr %in% special_chr
}

.append_special_missing_bins <- function(bin_table, y_all, is_special, is_missing, eps) {
  rows <- bin_table[, c("bin", "count", "event", "non_event"), drop = FALSE]

  add_row <- function(label, mask) {
    if (!any(mask)) {
      return(NULL)
    }
    event <- sum(y_all[mask] == 1)
    non_event <- sum(y_all[mask] == 0)
    data.frame(
      bin = label,
      count = event + non_event,
      event = event,
      non_event = non_event,
      stringsAsFactors = FALSE
    )
  }

  special_row <- add_row("Special", is_special)
  missing_row <- add_row("Missing", is_missing)

  if (!is.null(special_row)) {
    rows <- rbind(rows, special_row)
  }
  if (!is.null(missing_row)) {
    rows <- rbind(rows, missing_row)
  }

  .recompute_woe_from_counts(rows, eps = eps)
}

.recompute_woe_from_counts <- function(df_counts, eps) {
  total_event <- sum(df_counts$event)
  total_non_event <- sum(df_counts$non_event)

  dist_event <- pmax(df_counts$event / total_event, eps)
  dist_non_event <- pmax(df_counts$non_event / total_non_event, eps)
  woe <- log(dist_non_event / dist_event)
  iv <- (dist_non_event - dist_event) * woe

  out <- df_counts
  out$event_rate <- ifelse(out$count > 0, out$event / out$count, NA_real_)
  out$woe <- woe
  out$iv <- iv
  out
}

.build_prebreaks <- function(x, y, mode = "binary", method, max_n_prebins, min_bin_count) {
  if (identical(method, "quantile")) {
    probs <- seq(0, 1, length.out = max_n_prebins + 1L)
    q <- as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7))
    return(unique(q))
  }

  # 1D CART prebinning mirroring sklearn's best-first growth with max_leaf_nodes.
  ord <- order(x)
  x_s <- x[ord]
  y_s <- y[ord]
  n <- length(x_s)
  min_leaf <- max(1L, as.integer(min_bin_count))
  max_leaf_nodes <- max(2L, as.integer(max_n_prebins))

  if (identical(mode, "continuous")) {
    cum_y <- c(0, cumsum(as.numeric(y_s)))
    cum_y2 <- c(0, cumsum(as.numeric(y_s)^2))
    seg_sse <- function(l, r) {
      n_seg <- r - l + 1L
      sy <- cum_y[r + 1L] - cum_y[l]
      sy2 <- cum_y2[r + 1L] - cum_y2[l]
      sy2 - (sy * sy) / n_seg
    }
  } else {
    classes <- sort(unique(as.numeric(y_s)))
    y_idx <- match(as.numeric(y_s), classes)
    k <- length(classes)
    one_hot <- matrix(0, nrow = n, ncol = k)
    one_hot[cbind(seq_len(n), y_idx)] <- 1
    cum_counts <- apply(one_hot, 2, cumsum)
    cum_counts <- rbind(rep(0, k), cum_counts)
    seg_counts <- function(l, r) cum_counts[r + 1L, , drop = FALSE] - cum_counts[l, , drop = FALSE]
    gini_vec <- function(counts) {
      size <- sum(counts)
      if (size <= 0) {
        return(0)
      }
      p <- counts / size
      1 - sum(p^2)
    }
  }

  best_split <- function(l, r) {
    node_n <- r - l + 1L
    if (node_n < 2L * min_leaf) {
      return(NULL)
    }
    if (identical(mode, "continuous")) {
      parent_score <- seg_sse(l, r)
    } else {
      parent_score <- node_n * gini_vec(seg_counts(l, r))
    }

    lo <- l + min_leaf - 1L
    hi <- r - min_leaf
    if (lo > hi) {
      return(NULL)
    }

    best_gain <- -Inf
    best_i <- NA_integer_
    for (i in lo:hi) {
      if (x_s[i] >= x_s[i + 1L]) {
        next
      }
      n_l <- i - l + 1L
      n_r <- node_n - n_l
      if (identical(mode, "continuous")) {
        left_score <- seg_sse(l, i)
        right_score <- seg_sse(i + 1L, r)
        gain <- parent_score - left_score - right_score
      } else {
        left_score <- n_l * gini_vec(seg_counts(l, i))
        right_score <- n_r * gini_vec(seg_counts(i + 1L, r))
        gain <- parent_score - left_score - right_score
      }
      if (gain > best_gain + 1e-12) {
        best_gain <- gain
        best_i <- i
      }
    }

    if (!is.finite(best_gain) || is.na(best_i) || best_gain <= 0) {
      return(NULL)
    }

    list(
      gain = best_gain,
      split_index = best_i,
      threshold = (x_s[best_i] + x_s[best_i + 1L]) / 2
    )
  }

  leaves <- list(list(l = 1L, r = n, split = best_split(1L, n)))
  thresholds <- numeric(0)

  while (length(leaves) < max_leaf_nodes) {
    gains <- vapply(leaves, function(node) if (is.null(node$split)) -Inf else node$split$gain, numeric(1))
    k <- which.max(gains)
    if (length(k) == 0L || !is.finite(gains[k]) || gains[k] <= 0) {
      break
    }

    node <- leaves[[k]]
    split <- node$split
    thresholds <- c(thresholds, split$threshold)

    left <- list(l = node$l, r = split$split_index, split = best_split(node$l, split$split_index))
    right <- list(l = split$split_index + 1L, r = node$r, split = best_split(split$split_index + 1L, node$r))

    leaves <- append(leaves[-k], list(left, right))
  }

  thresholds <- sort(unique(thresholds))
  if (length(thresholds) > (max_leaf_nodes - 1L)) {
    thresholds <- thresholds[seq_len(max_leaf_nodes - 1L)]
  }

  unique(c(min(x), thresholds, max(x)))
}

.refine_splits_remove_pure <- function(splits, x, y) {
  splits <- sort(unique(as.numeric(splits)))
  if (length(splits) == 0L) {
    return(splits)
  }

  idx <- findInterval(x, splits)
  n_bins <- length(splits) + 1L
  n_event <- numeric(n_bins)
  n_nonevent <- numeric(n_bins)

  for (i in 0:(n_bins - 1L)) {
    mask <- idx == i
    n_event[i + 1L] <- sum(y[mask] == 1)
    n_nonevent[i + 1L] <- sum(y[mask] == 0)
  }

  mask_remove <- (n_event == 0) | (n_nonevent == 0)
  if (!any(mask_remove)) {
    return(splits)
  }

  if (n_bins <= 2L) {
    return(numeric(0))
  }

  mask_splits <- c(mask_remove[1:(n_bins - 2L)], mask_remove[n_bins - 1L] | mask_remove[n_bins])
  refined <- splits[!mask_splits]
  .refine_splits_remove_pure(refined, x, y)
}

.refine_splits_remove_pure_multiclass <- function(splits, x, y) {
  splits <- sort(unique(as.numeric(splits)))
  if (length(splits) == 0L) {
    return(splits)
  }

  idx <- findInterval(x, splits)
  n_bins <- length(splits) + 1L
  classes <- sort(unique(y))
  mask_remove <- rep(FALSE, n_bins)

  for (i in 0:(n_bins - 1L)) {
    mask <- idx == i
    bin_n <- sum(mask)
    if (bin_n == 0L) {
      mask_remove[i + 1L] <- TRUE
      next
    }
    counts <- vapply(classes, function(cl) sum(y[mask] == cl), numeric(1))
    # For each class c, n_event>0 and n_nonevent>0 <=> count_c in (0, bin_n)
    if (any(counts == 0) || any(counts == bin_n)) {
      mask_remove[i + 1L] <- TRUE
    }
  }

  if (!any(mask_remove)) {
    return(splits)
  }
  if (n_bins <= 2L) {
    return(numeric(0))
  }

  mask_splits <- c(mask_remove[1:(n_bins - 2L)], mask_remove[n_bins - 1L] | mask_remove[n_bins])
  refined <- splits[!mask_splits]
  .refine_splits_remove_pure_multiclass(refined, x, y)
}

.n_peaks_valleys <- function(v) {
  if (length(v) < 3L) {
    return(0L)
  }
  d <- sign(diff(v))
  sum(d[-1] != d[-length(d)])
}

.extreme_points_area <- function(v) {
  n <- length(v)
  if (n <= 2L) {
    return(0)
  }
  pos_min <- which.min(v)
  pos_max <- which.max(v)
  ymin <- v[pos_min]
  ymax <- v[pos_max]
  if (abs(ymax - ymin) < 1e-12) {
    return(0)
  }
  xinit <- 0
  xmin <- pos_min - 1L
  xmax <- pos_max - 1L
  xlast <- n - 1L
  yinit <- v[1L]
  ylast <- v[n]
  area1 <- 0.5 * abs(det(matrix(c(xinit, xmin, xmax, yinit, ymin, ymax, 1, 1, 1), nrow = 3, byrow = TRUE)))
  area2 <- 0.5 * abs(det(matrix(c(xmin, xmax, xlast, ymin, ymax, ylast, 1, 1, 1), nrow = 3, byrow = TRUE)))
  (area1 + area2) / ((ymax - ymin) * n)
}

.convex_hull_ratio <- function(v) {
  n <- length(v)
  if (n <= 2L) {
    return(0)
  }
  ymin <- min(v)
  ymax <- max(v)
  if (abs(ymax - ymin) < 1e-12) {
    return(0)
  }
  x <- seq_len(n) - 1L
  idx <- grDevices::chull(x, v)
  idx <- c(idx, idx[1L])
  area <- 0.5 * abs(sum(x[idx[-1L]] * v[idx[-length(idx)]] - x[idx[-length(idx)]] * v[idx[-1L]]))
  area / ((ymax - ymin) * n)
}

.auto_monotonic_decision <- function(lr_sense, p_records_min_left, p_records_min_right, p_records_max_left, p_records_max_right, p_area, p_convex_hull) {
  if (p_area <= 0.22145836800336838) {
    if (lr_sense == 0) {
      if (p_convex_hull <= 0.48331470787525177) {
        if (p_records_min_right <= 0.010740397498011589) {
          monotonic_trend <- 1
        } else {
          if (p_records_min_right <= 0.022145185619592667) monotonic_trend <- 3 else monotonic_trend <- 1
        }
      } else {
        if (p_records_max_right <= 0.6426683664321899) monotonic_trend <- 3 else monotonic_trend <- 1
      }
    } else {
      monotonic_trend <- 0
    }
  } else {
    if (p_records_min_right <= 0.06137961149215698) {
      if (p_convex_hull <= 0.23837491869926453) {
        monotonic_trend <- 1
      } else {
        if (p_records_max_left <= 0.10170064494013786) {
          if (p_records_max_left <= 0.01817034650593996) monotonic_trend <- 3 else monotonic_trend <- 1
        } else {
          monotonic_trend <- 2
        }
      }
    } else {
      if (p_records_min_left <= 0.05336669087409973) {
        if (p_records_max_right <= 0.0695494469255209) {
          monotonic_trend <- 0
        } else {
          if (p_records_max_left <= 0.14705360680818558) monotonic_trend <- 0 else monotonic_trend <- 2
        }
      } else {
        if (p_records_min_left <= 0.8308950066566467) {
          monotonic_trend <- 3
        } else {
          if (p_records_max_right <= 0.1587613895535469) monotonic_trend <- 3 else monotonic_trend <- 2
        }
      }
    }
  }
  c("ascending", "descending", "peak", "valley")[monotonic_trend + 1L]
}

.auto_monotonic_asc_desc_decision <- function(p_trend_changes, lr_sense, p_records_min_left, p_records_min_right, p_records_max_left, p_records_max_right, p_area, p_convex_hull) {
  if (lr_sense == 0) {
    if (p_area <= 0.4890555590391159) {
      if (p_records_max_right <= 0.029244758188724518) monotonic_trend <- 0 else monotonic_trend <- 1
    } else {
      if (p_convex_hull <= 0.5553120970726013) monotonic_trend <- 0 else monotonic_trend <- 1
    }
  } else {
    if (p_records_max_left <= 0.03698493912816048) {
      monotonic_trend <- 1
    } else {
      if (p_records_min_left <= 0.7991077601909637) {
        if (p_area <= 0.48206718266010284) {
          monotonic_trend <- 0
        } else {
          if (p_records_max_left <= 0.8631451725959778) monotonic_trend <- 0 else monotonic_trend <- 1
        }
      } else {
        if (p_trend_changes <= 0.5277777910232544) {
          if (p_records_min_left <= 0.8155287206172943) monotonic_trend <- 1 else monotonic_trend <- 0
        } else {
          monotonic_trend <- 1
        }
      }
    }
  }
  if (monotonic_trend == 0) "ascending" else "descending"
}

.auto_monotonic_from_values <- function(n_records, values, auto_mode = "auto") {
  n_prebins <- length(values)
  if (n_prebins < 2L) {
    return("descending")
  }
  p_trend_changes <- .n_peaks_valleys(values) / n_prebins
  lr_coef <- stats::coef(stats::lm(values ~ I(seq_len(n_prebins) - 1L)))[2]
  lr_sense <- as.integer(lr_coef > 0)
  pos_min <- which.min(values)
  pos_max <- which.max(values)
  n1 <- max(1, n_prebins - 1L)
  left_min_idx <- if (pos_min > 1L) seq_len(pos_min - 1L) else integer(0)
  right_min_idx <- if (pos_min < n_prebins) (pos_min + 1L):n_prebins else integer(0)
  left_max_idx <- if (pos_max > 1L) seq_len(pos_max - 1L) else integer(0)
  right_max_idx <- if (pos_max < n_prebins) (pos_max + 1L):n_prebins else integer(0)
  p_records_min_left <- sum(n_records[left_min_idx]) / sum(n_records)
  p_records_min_right <- sum(n_records[right_min_idx]) / sum(n_records)
  p_records_max_left <- sum(n_records[left_max_idx]) / sum(n_records)
  p_records_max_right <- sum(n_records[right_max_idx]) / sum(n_records)
  p_area <- .extreme_points_area(values)
  p_convex_hull <- .convex_hull_ratio(values)
  if (auto_mode %in% c("auto", "auto_heuristic")) {
    .auto_monotonic_decision(lr_sense, p_records_min_left, p_records_min_right, p_records_max_left, p_records_max_right, p_area, p_convex_hull)
  } else if (auto_mode == "auto_asc_desc") {
    .auto_monotonic_asc_desc_decision(p_trend_changes, lr_sense, p_records_min_left, p_records_min_right, p_records_max_left, p_records_max_right, p_area, p_convex_hull)
  } else {
    auto_mode
  }
}

.check_monotonic <- function(v, trend) {
  if (is.null(trend) || trend %in% c("none", "")) {
    return(TRUE)
  }
  if (trend == "ascending") {
    return(all(diff(v) >= -1e-12))
  }
  if (trend == "descending") {
    return(all(diff(v) <= 1e-12))
  }
  if (trend %in% c("peak", "peak_heuristic")) {
    n <- length(v)
    for (t in seq_len(n)) {
      if (all(diff(v[seq_len(t)]) >= -1e-12) && all(diff(v[t:n]) <= 1e-12)) return(TRUE)
    }
    return(FALSE)
  }
  if (trend %in% c("valley", "valley_heuristic")) {
    n <- length(v)
    for (t in seq_len(n)) {
      if (all(diff(v[seq_len(t)]) <= 1e-12) && all(diff(v[t:n]) >= -1e-12)) return(TRUE)
    }
    return(FALSE)
  }
  TRUE
}

.fit_optimal <- function(
    x, y, max_n_bins, prebinning_method, max_n_prebins, min_bin_count,
    min_bin_n_event, min_bin_n_nonevent, monotonic_trend, eps
) {
  pre_breaks <- .build_prebreaks(
    x = x,
    y = y,
    mode = "binary",
    method = prebinning_method,
    max_n_prebins = max_n_prebins,
    min_bin_count = min_bin_count
  )
  if (length(pre_breaks) > 2L) {
    refined_splits <- .refine_splits_remove_pure(pre_breaks[2:(length(pre_breaks) - 1L)], x, y)
    pre_breaks <- unique(c(min(x), refined_splits, max(x)))
  }
  if (length(pre_breaks) < 2L) {
    fallback_bins <- if (is.na(max_n_bins)) 2L else min(max_n_bins, 2L)
    return(.fit_quantile(x, y, max_n_bins = fallback_bins, eps = eps))
  }
  pre_breaks <- .expand_boundaries(pre_breaks)

  pre_bins <- cut(x, breaks = pre_breaks, include.lowest = TRUE, right = TRUE)
  lv <- levels(pre_bins)
  m <- length(lv)

  event <- numeric(m)
  non_event <- numeric(m)
  count <- numeric(m)

  for (i in seq_len(m)) {
    idx <- which(pre_bins == lv[i])
    if (length(idx) > 0L) {
      event[i] <- sum(y[idx] == 1)
      non_event[i] <- sum(y[idx] == 0)
      count[i] <- length(idx)
    }
  }

  # Remove empty pre-bins to avoid pathological partitions.
  keep_pre <- count > 0
  event <- event[keep_pre]
  non_event <- non_event[keep_pre]
  count <- count[keep_pre]
  if (!all(keep_pre)) {
    idx_keep <- which(keep_pre)
    pre_breaks <- c(pre_breaks[min(idx_keep)], pre_breaks[idx_keep + 1L])
    pre_breaks <- unique(pre_breaks)
  }

  m <- length(count)
  if (m < 1L) {
    fallback_bins <- if (is.na(max_n_bins)) 2L else min(max_n_bins, 2L)
    return(.fit_quantile(x, y, max_n_bins = fallback_bins, eps = eps))
  }
  max_bins_eff <- if (is.na(max_n_bins)) m else min(max_n_bins, m)

  total_event <- sum(event)
  total_non_event <- sum(non_event)

  cum_event <- c(0, cumsum(event))
  cum_non_event <- c(0, cumsum(non_event))
  cum_count <- c(0, cumsum(count))

  segment_stats <- function(i, j) {
    seg_event <- cum_event[j + 1L] - cum_event[i]
    seg_non_event <- cum_non_event[j + 1L] - cum_non_event[i]
    seg_count <- cum_count[j + 1L] - cum_count[i]

    dist_event <- max(seg_event / total_event, eps)
    dist_non_event <- max(seg_non_event / total_non_event, eps)
    seg_woe <- log(dist_non_event / dist_event)
    seg_iv <- (dist_non_event - dist_event) * seg_woe
    seg_rate <- if (seg_count > 0) seg_event / seg_count else NA_real_

    list(
      event = seg_event,
      non_event = seg_non_event,
      count = seg_count,
      woe = seg_woe,
      iv = seg_iv,
      rate = seg_rate
    )
  }

  eval_partition <- function(ends, trend) {
    starts <- c(1L, utils::head(ends + 1L, -1L))
    stats_list <- vector("list", length(ends))

    for (k in seq_along(ends)) {
      st <- starts[k]
      en <- ends[k]
      s <- segment_stats(st, en)
      if (s$count < min_bin_count) {
        return(NULL)
      }
      if (s$event < min_bin_n_event || s$non_event < min_bin_n_nonevent) {
        return(NULL)
      }
      stats_list[[k]] <- s
    }

    rates <- vapply(stats_list, function(z) z$rate, numeric(1))
    if (identical(trend, "ascending") && any(diff(rates) < -1e-12)) {
      return(NULL)
    }
    if (identical(trend, "descending") && any(diff(rates) > 1e-12)) {
      return(NULL)
    }

    list(ends = ends, stats = stats_list, objective = sum(vapply(stats_list, function(z) z$iv, numeric(1))))
  }

  explore_partitions <- function(trend) {
    best <- NULL

    recurse <- function(start_idx, bins_used, ends) {
      if (start_idx > m) {
        if (bins_used >= 1L && bins_used <= max_bins_eff) {
          cand <- eval_partition(ends, trend)
          if (!is.null(cand) && (is.null(best) || cand$objective > best$objective)) {
            best <<- cand
          }
        }
        return(invisible(NULL))
      }

      if (bins_used >= max_bins_eff) {
        return(invisible(NULL))
      }

      for (end_idx in seq.int(start_idx, m)) {
        recurse(end_idx + 1L, bins_used + 1L, c(ends, end_idx))
      }
    }

    recurse(1L, 0L, integer(0))
    best
  }

  trend_candidates <- if (identical(monotonic_trend, "auto")) c("ascending", "descending") else monotonic_trend

  best <- NULL
  selected_trend <- "none"
  for (trend in trend_candidates) {
    cand <- explore_partitions(trend)
    if (!is.null(cand) && (is.null(best) || cand$objective > best$objective)) {
      best <- cand
      selected_trend <- trend
    }
  }

  if (is.null(best) && identical(monotonic_trend, "auto")) {
    best <- explore_partitions("none")
    selected_trend <- "none"
  }
  if (is.null(best)) {
    warning("No feasible optimal partition was found with current constraints. Falling back to quantile.")
    return(.fit_quantile(x, y, max_n_bins = if (is.na(max_n_bins)) 10L else max_n_bins, eps = eps))
  }

  starts <- c(1L, utils::head(best$ends + 1L, -1L))
  bins_n <- length(best$ends)
  labels <- character(bins_n)

  final_breaks <- numeric(bins_n + 1L)
  final_breaks[1] <- pre_breaks[1]
  for (k in seq_len(bins_n)) {
    start_pre <- starts[k]
    end_pre <- best$ends[k]
    lower <- pre_breaks[start_pre]
    upper <- pre_breaks[end_pre + 1L]
    labels[k] <- .format_interval_label(lower, upper, include_lowest = (k == 1L))
    final_breaks[k + 1L] <- upper
  }
  final_breaks <- .expand_boundaries(final_breaks)

  seg_event <- vapply(best$stats, function(z) z$event, numeric(1))
  seg_non_event <- vapply(best$stats, function(z) z$non_event, numeric(1))
  seg_count <- vapply(best$stats, function(z) z$count, numeric(1))
  seg_woe <- vapply(best$stats, function(z) z$woe, numeric(1))
  seg_iv <- vapply(best$stats, function(z) z$iv, numeric(1))
  seg_rate <- vapply(best$stats, function(z) z$rate, numeric(1))

  bin_table <- data.frame(
    bin = labels,
    count = seg_count,
    event = seg_event,
    non_event = seg_non_event,
    event_rate = seg_rate,
    woe = seg_woe,
    iv = seg_iv,
    stringsAsFactors = FALSE
  )

  list(breaks = final_breaks, bin_table = bin_table, monotonic_trend = selected_trend)
}

.fit_optimal_localsolver <- function(
    x, y, max_n_bins, prebinning_method, max_n_prebins, min_bin_count,
    min_bin_n_event, min_bin_n_nonevent, monotonic_trend, eps
) {
  # LocalSolver-compatible path: deterministic native solve with tuned defaults.
  .fit_optimal(
    x = x,
    y = y,
    max_n_bins = max_n_bins,
    prebinning_method = prebinning_method,
    max_n_prebins = max_n_prebins,
    min_bin_count = min_bin_count,
    min_bin_n_event = min_bin_n_event,
    min_bin_n_nonevent = min_bin_n_nonevent,
    monotonic_trend = monotonic_trend,
    eps = eps
  )
}

.fit_optimal_large_scale <- function(
    x, y, max_n_bins, max_n_prebins, min_bin_count,
    min_bin_n_event, min_bin_n_nonevent, monotonic_trend, eps
) {
  # Large-scale profile: quantile prebinning + strict prebin cap for tractable search.
  n <- length(x)
  prebins_eff <- min(max(10L, as.integer(max_n_prebins)), 20L, max(10L, floor(sqrt(n) / 2)))
  .fit_optimal(
    x = x,
    y = y,
    max_n_bins = max_n_bins,
    prebinning_method = "quantile",
    max_n_prebins = prebins_eff,
    min_bin_count = min_bin_count,
    min_bin_n_event = min_bin_n_event,
    min_bin_n_nonevent = min_bin_n_nonevent,
    monotonic_trend = monotonic_trend,
    eps = eps
  )
}

.python_optbinning_available <- function(py_solver = "cp") {
  py_override <- Sys.getenv("OPTBINNING_PYTHON", unset = "")
  py <- if (nzchar(py_override)) py_override else Sys.which("python3")
  if (identical(py, "") || !file.exists(py)) {
    return(FALSE)
  }
  py_script <- tempfile(fileext = ".py")
  script <- c(
    "import numpy as np",
    "import sys",
    "from optbinning import OptimalBinning",
    "solver = sys.argv[1]",
    "x=np.array([0.,1.,2.,3.,4.,5.])",
    "y=np.array([0,0,0,1,1,1])",
    "ob=OptimalBinning(name='x', solver=solver, max_n_prebins=4, max_n_bins=3)",
    "ob.fit(x,y)",
    "print(ob.status)"
  )
  writeLines(script, py_script)
  status <- suppressWarnings(system2(py, c(py_script, py_solver), stdout = FALSE, stderr = FALSE))
  identical(status, 0L)
}

.fit_optimal_python_solver <- function(
    x, y, name, max_n_bins, prebinning_method, max_n_prebins, min_bin_count,
    min_bin_n_event, min_bin_n_nonevent, monotonic_trend, eps, py_solver
) {
  py_override <- Sys.getenv("OPTBINNING_PYTHON", unset = "")
  py <- if (nzchar(py_override)) py_override else Sys.which("python3")
  if (identical(py, "")) {
    stop("`python3` was not found. Use solver = 'native' or install Python.", call. = FALSE)
  }
  if (!file.exists(py)) {
    stop("`OPTBINNING_PYTHON` does not point to a valid executable.", call. = FALSE)
  }

  input_csv <- tempfile(fileext = ".csv")
  output_splits <- tempfile(fileext = ".csv")
  output_bins <- tempfile(fileext = ".csv")
  output_status <- tempfile(fileext = ".txt")
  py_script <- tempfile(fileext = ".py")

  utils::write.csv(data.frame(x = x, y = y), input_csv, row.names = FALSE)

  min_prebin_size <- min(1, max(1 / length(x), min_bin_count / length(x)))
  min_event_count <- max(0L, as.integer(min_bin_n_event))
  min_nonevent_count <- max(0L, as.integer(min_bin_n_nonevent))
  trend <- monotonic_trend

  max_bins_arg <- if (is.na(max_n_bins)) "None" else as.character(max_n_bins)

  script <- c(
    "import pandas as pd",
    "import numpy as np",
    "import sys",
    "from optbinning import OptimalBinning",
    "inp, out_splits, out_bins, out_status, prebin_method, max_prebins, min_prebin_size, min_event_count, min_nonevent_count, trend, max_bins = sys.argv[1:12]",
    "max_prebins = int(max_prebins)",
    "min_prebin_size = float(min_prebin_size)",
    "min_event_count = int(min_event_count)",
    "min_nonevent_count = int(min_nonevent_count)",
    "max_bins = None if max_bins == 'None' else int(max_bins)",
    "df = pd.read_csv(inp)",
    "x = df['x'].to_numpy()",
    "y = df['y'].to_numpy()",
    sprintf("optb = OptimalBinning(name=%s, dtype='numerical', solver=%s, prebinning_method=prebin_method, max_n_prebins=max_prebins, min_prebin_size=min_prebin_size, min_bin_n_event=min_event_count, min_bin_n_nonevent=min_nonevent_count, monotonic_trend=trend, max_n_bins=max_bins)", deparse(name), deparse(py_solver)),
    "optb.fit(x, y)",
    "splits = np.asarray(optb.splits, dtype=float) if optb.splits is not None else np.array([])",
    "pd.DataFrame({'split': splits}).to_csv(out_splits, index=False)",
    "bt = optb.binning_table.build()",
    "core = bt.copy()",
    "if 'Bin' in core.columns:",
    "    core = core.loc[~core['Bin'].isin(['Special', 'Missing'])]",
    "if 'Totals' in core.index:",
    "    core = core.loc[core.index != 'Totals']",
    "core_out = pd.DataFrame({",
    "    'count': pd.to_numeric(core['Count'], errors='coerce'),",
    "    'event': pd.to_numeric(core['Event'], errors='coerce'),",
    "    'non_event': pd.to_numeric(core['Non-event'], errors='coerce'),",
    "    'event_rate': pd.to_numeric(core['Event rate'], errors='coerce'),",
    "    'woe': pd.to_numeric(core['WoE'], errors='coerce'),",
    "    'iv': pd.to_numeric(core['IV'], errors='coerce')",
    "}).dropna()",
    "core_out.to_csv(out_bins, index=False)",
    "with open(out_status, 'w', encoding='utf-8') as f:",
    "    f.write(str(optb.status))"
  )
  writeLines(script, py_script)

  res <- system2(
    py,
    args = c(
      py_script, input_csv, output_splits, output_bins, output_status,
      prebinning_method,
      as.character(max_n_prebins), as.character(min_prebin_size),
      as.character(min_event_count), as.character(min_nonevent_count),
      trend, max_bins_arg
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(res, "status")
  if (!is.null(status) && status != 0) {
    stop(
      paste0(
        "Python solver backend failed. Install Python package `optbinning` and configured solver, or use solver = 'native'.\n",
        paste(res, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  if (!file.exists(output_splits) || !file.exists(output_bins)) {
    stop("Python solver backend did not generate expected output files.", call. = FALSE)
  }

  split_df <- utils::read.csv(output_splits)
  py_bins <- utils::read.csv(output_bins)
  splits <- if ("split" %in% names(split_df)) as.numeric(split_df$split) else numeric(0)
  splits <- splits[is.finite(splits)]
  splits <- sort(unique(splits))

  if (length(splits) == 0L) {
    return(.fit_quantile(x, y, max_n_bins = min(max_n_bins, 2L), eps = eps))
  }

  breaks <- c(-Inf, splits, Inf)
  bins <- cut(x, breaks = breaks, include.lowest = TRUE, right = TRUE)
  levels_bins <- levels(bins)

  if (nrow(py_bins) == length(levels_bins)) {
    bin_table <- data.frame(
      bin = levels_bins,
      count = as.numeric(py_bins$count),
      event = as.numeric(py_bins$event),
      non_event = as.numeric(py_bins$non_event),
      event_rate = as.numeric(py_bins$event_rate),
      woe = as.numeric(py_bins$woe),
      iv = as.numeric(py_bins$iv),
      stringsAsFactors = FALSE
    )
  } else {
    bin_table <- .bin_table_from_factor_bins(bins, y, eps)
  }

  selected_trend <- monotonic_trend
  if (file.exists(output_status)) {
    py_status <- trimws(paste(readLines(output_status, warn = FALSE), collapse = ""))
    if (nzchar(py_status) && !identical(py_status, "OPTIMAL")) {
      warning(paste0("Python solver status: ", py_status))
    }
  }

  list(breaks = breaks, bin_table = bin_table, monotonic_trend = selected_trend)
}

.fit_optimal_python_cp <- function(
    x, y, name, max_n_bins, prebinning_method, max_n_prebins, min_bin_count,
    min_bin_n_event, min_bin_n_nonevent, monotonic_trend, eps
) {
  .fit_optimal_python_solver(
    x = x, y = y, name = name,
    max_n_bins = max_n_bins, prebinning_method = prebinning_method,
    max_n_prebins = max_n_prebins, min_bin_count = min_bin_count,
    min_bin_n_event = min_bin_n_event, min_bin_n_nonevent = min_bin_n_nonevent,
    monotonic_trend = monotonic_trend, eps = eps, py_solver = "cp"
  )
}

.fit_optimal_python_ls <- function(
    x, y, name, max_n_bins, prebinning_method, max_n_prebins, min_bin_count,
    min_bin_n_event, min_bin_n_nonevent, monotonic_trend, eps
) {
  .fit_optimal_python_solver(
    x = x, y = y, name = name,
    max_n_bins = max_n_bins, prebinning_method = prebinning_method,
    max_n_prebins = max_n_prebins, min_bin_count = min_bin_count,
    min_bin_n_event = min_bin_n_event, min_bin_n_nonevent = min_bin_n_nonevent,
    monotonic_trend = monotonic_trend, eps = eps, py_solver = "ls"
  )
}

.expand_boundaries <- function(breaks) {
  breaks <- as.numeric(breaks)
  breaks[1] <- breaks[1] - .Machine$double.eps
  breaks[length(breaks)] <- breaks[length(breaks)] + .Machine$double.eps
  breaks
}

.format_interval_label <- function(lower, upper, include_lowest = FALSE) {
  left <- if (include_lowest) "[" else "("
  paste0(left, format(lower, trim = TRUE), ",", format(upper, trim = TRUE), "]")
}

.bin_table_from_factor_bins <- function(bins, y_num, eps) {
  total_event <- sum(y_num == 1)
  total_non_event <- sum(y_num == 0)

  levels_bins <- levels(bins)
  event <- numeric(length(levels_bins))
  non_event <- numeric(length(levels_bins))

  for (i in seq_along(levels_bins)) {
    idx <- which(bins == levels_bins[i])
    event[i] <- sum(y_num[idx] == 1)
    non_event[i] <- sum(y_num[idx] == 0)
  }

  dist_event <- pmax(event / total_event, eps)
  dist_non_event <- pmax(non_event / total_non_event, eps)
  woe <- log(dist_non_event / dist_event)
  iv <- (dist_non_event - dist_event) * woe

  data.frame(
    bin = levels_bins,
    count = event + non_event,
    event = event,
    non_event = non_event,
    event_rate = ifelse((event + non_event) > 0, event / (event + non_event), NA_real_),
    woe = woe,
    iv = iv,
    stringsAsFactors = FALSE
  )
}

#' Transform x to Weight of Evidence (WOE)
#'
#' @param object A fitted `OptimalBinning` object.
#' @param x Numeric vector to transform.
#' @param metric Output metric. Supported values include `"woe"`, `"event_rate"`
#'   and `"bins"` (bin label assignment).
#' @return Numeric vector with WOE values.
#' @export
transform <- function(object, x, metric = NULL, ...) {
  UseMethod("transform")
}

#' @export
transform.OptimalBinning <- function(object, x, metric = NULL, ...) {
  if (!isTRUE(object$fitted)) {
    stop("Model is not fitted. Call fit() first.", call. = FALSE)
  }
  if (identical(object$dtype, "numerical") && !is.numeric(x)) {
    stop("`x` must be numeric for dtype = 'numerical'.", call. = FALSE)
  }
  if (identical(object$dtype, "categorical") && !(is.character(x) || is.factor(x))) {
    stop("`x` must be character/factor for dtype = 'categorical'.", call. = FALSE)
  }

  if (is.null(metric)) {
    metric <- if (identical(object$target_dtype, "binary")) "woe" else "event_rate"
  }
  if (!metric %in% c("woe", "event_rate", "bins")) {
    stop("`metric` must be one of 'woe', 'event_rate' or 'bins'.", call. = FALSE)
  }

  if (identical(metric, "bins")) {
    out_bins <- rep(NA_character_, length(x))
    if (identical(object$dtype, "categorical")) {
      x_chr <- as.character(x)
      known <- as.character(object$bin_table$bin)
      out_bins <- ifelse(x_chr %in% known, x_chr, NA_character_)
      if ("Missing" %in% known) out_bins[is.na(x)] <- "Missing"
      special_mask <- .is_special(x, object$special_codes)
      if ("Special" %in% known) out_bins[special_mask] <- "Special"
      return(out_bins)
    }
    k <- length(object$breaks) - 1L
    core_labels <- as.character(object$bin_table$bin[seq_len(min(k, nrow(object$bin_table)))])
    idx_non_missing <- which(!is.na(x))
    if (length(idx_non_missing) > 0L) {
      bin_idx <- findInterval(x[idx_non_missing], object$breaks, rightmost.closed = TRUE)
      in_range <- bin_idx >= 1L & bin_idx <= length(core_labels)
      out_bins[idx_non_missing[in_range]] <- core_labels[bin_idx[in_range]]
    }
    if ("Missing" %in% object$bin_table$bin) out_bins[is.na(x)] <- "Missing"
    special_mask <- .is_special(x, object$special_codes)
    if ("Special" %in% object$bin_table$bin) out_bins[special_mask] <- "Special"
    return(out_bins)
  }

  map <- stats::setNames(object$bin_table$woe, object$bin_table$bin)
  map_rate <- stats::setNames(object$bin_table$event_rate, object$bin_table$bin)
  out <- rep(NA_real_, length(x))

  if (identical(object$dtype, "categorical")) {
    x_chr <- as.character(x)
    base_map <- if (identical(metric, "woe")) map else map_rate
    out <- as.numeric(base_map[x_chr])
    if ("Missing" %in% names(base_map)) {
      out[is.na(x)] <- base_map[["Missing"]]
    } else {
      out[is.na(x)] <- NA_real_
    }
    special_mask <- .is_special(x, object$special_codes)
    if ("Special" %in% names(base_map)) {
      out[special_mask] <- base_map[["Special"]]
    }
    return(out)
  }

  # Use interval indexing instead of textual bin labels, which may differ by
  # rounding/formatting from cut() labels.
  k <- length(object$breaks) - 1L
  base_vals <- if (identical(metric, "woe")) object$bin_table$woe else object$bin_table$event_rate
  interval_vals <- base_vals[seq_len(k)]
  idx_non_missing <- which(!is.na(x))
  if (length(idx_non_missing) > 0L) {
    bin_idx <- findInterval(x[idx_non_missing], object$breaks, rightmost.closed = TRUE)
    in_range <- bin_idx >= 1L & bin_idx <= k
    out[idx_non_missing[in_range]] <- interval_vals[bin_idx[in_range]]
  }
  use_map <- if (identical(metric, "woe")) map else map_rate
  if ("Missing" %in% names(use_map)) {
    out[is.na(x)] <- use_map[["Missing"]]
  } else {
    out[is.na(x)] <- NA_real_
  }
  special_mask <- .is_special(x, object$special_codes)
  if ("Special" %in% names(use_map)) {
    out[special_mask] <- use_map[["Special"]]
  }

  out
}

#' Print model information
#'
#' @param object A fitted model object.
#' @param print_level Integer verbosity level.
#' @return Invisibly returns a summary list.
#' @export
information <- function(object, print_level = 1L) {
  UseMethod("information")
}

#' @export
information.OptimalBinning <- function(object, print_level = 1L) {
  if (!isTRUE(object$fitted)) stop("Model is not fitted. Call fit() first.", call. = FALSE)
  info <- list(
    name = object$name,
    dtype = object$dtype,
    target_dtype = object$target_dtype,
    status = if (!is.null(object$status)) object$status else "OPTIMAL",
    n_bins = object$n_bins,
    n_splits = if (is.null(object$splits)) 0L else length(object$splits),
    monotonic_trend = object$fit_params$monotonic_trend,
    solver = object$fit_params$solver
  )
  if (isTRUE(as.integer(print_level) >= 1L)) {
    cat("OptimalBinning Information\n")
    cat("  Name:", info$name, "\n")
    cat("  Status:", info$status, "\n")
    cat("  Bins:", info$n_bins, "\n")
    cat("  Splits:", info$n_splits, "\n")
    cat("  Target:", info$target_dtype, "\n")
    cat("  Solver:", info$solver, "\n")
  }
  invisible(info)
}

#' Analyze binning table
#'
#' @param object A fitted model object.
#' @return A list with basic diagnostics.
#' @export
analysis <- function(object) {
  UseMethod("analysis")
}

#' @export
analysis.OptimalBinning <- function(object) {
  if (!isTRUE(object$fitted)) stop("Model is not fitted. Call fit() first.", call. = FALSE)
  bt <- object$bin_table
  core <- bt[!(bt$bin %in% c("Special", "Missing")), , drop = FALSE]
  if (identical(object$target_dtype, "multiclass")) {
    ev_cols <- grep("^event_class_", names(core), value = TRUE)
    out <- list(target = "multiclass", n_bins = nrow(core), chi2_consecutive_pvalue = numeric(0))
    if (length(ev_cols) > 0 && nrow(core) > 1L) {
      pvals <- numeric(nrow(core) - 1L)
      for (i in seq_len(nrow(core) - 1L)) {
        tab <- rbind(as.numeric(core[i, ev_cols]), as.numeric(core[i + 1L, ev_cols]))
        pvals[i] <- tryCatch(suppressWarnings(stats::chisq.test(tab)$p.value), error = function(e) NA_real_)
      }
      out$chi2_consecutive_pvalue <- pvals
    }
    return(out)
  }
  if (identical(object$target_dtype, "continuous")) {
    return(list(target = "continuous", n_bins = nrow(core), mean = as.numeric(core$event_rate)))
  }
  list(target = "binary", n_bins = nrow(core), total_iv = sum(core$iv, na.rm = TRUE))
}

#' Fit model and transform x in one step
#'
#' @param object An `OptimalBinning` object.
#' @param x Numerical predictor vector.
#' @param y Binary target vector.
#' @param max_n_bins Maximum final bins.
#' @param algorithm Binning algorithm.
#' @param prebinning_method Prebinning method for optimal mode.
#' @param max_n_prebins Maximum pre-bins for the `"optimal"` algorithm.
#' @param min_bin_size Minimum bin size as count or fraction.
#' @param min_bin_n_event Minimum number of events required per bin (native solver).
#' @param min_bin_n_nonevent Minimum number of non-events required per bin (native solver).
#' @param monotonic_trend Monotonic trend constraint.
#' @param special_codes Optional values treated as `Special` bin.
#' @param solver Solver backend for `algorithm = "optimal"`.
#' @param profile Optimization profile (`"standard"` or `"large_scale"`).
#' @return List with `model` and `x_woe`.
#' @export
fit_transform <- function(
    object,
    x,
    y,
    max_n_bins = 10L,
    algorithm = "quantile",
    prebinning_method = "cart",
    max_n_prebins = 20L,
    min_bin_size = 0.05,
    min_bin_n_event = 1L,
    min_bin_n_nonevent = 1L,
    monotonic_trend = "none",
    special_codes = NULL,
    solver = "native",
    profile = "standard"
) {
  model <- fit(
    object,
    x,
    y,
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
    profile = profile
  )
  x_woe <- transform(model, x)
  list(model = model, x_woe = x_woe)
}

#' @export
print.OptimalBinning <- function(x, ...) {
  cat("OptimalBinning\n")
  cat("  Name:   ", x$name, "\n", sep = "")
  cat("  DType:  ", x$dtype, "\n", sep = "")
  cat("  Target: ", x$target_dtype, "\n", sep = "")
  cat("  Fitted: ", x$fitted, "\n", sep = "")
  if (isTRUE(x$fitted)) {
    cat("  Bins:   ", x$n_bins, "\n", sep = "")
    if (is.finite(x$total_iv)) {
      cat("  Total IV:", format(round(x$total_iv, 6), nsmall = 6), "\n")
    } else {
      cat("  Total IV: N/A\n")
    }
    if (!is.null(x$fit_params$algorithm)) {
      cat("  Algorithm: ", x$fit_params$algorithm, "\n", sep = "")
    }
    if (!is.null(x$fit_params$monotonic_trend)) {
      cat("  Monotonic Trend: ", x$fit_params$monotonic_trend, "\n", sep = "")
    }
    if (!is.null(x$fit_params$solver)) {
      cat("  Solver: ", x$fit_params$solver, "\n", sep = "")
    }
  }
  invisible(x)
}
