#' Create an Optimal Piecewise Binning model object
#'
#' @param name Feature name.
#' @param target_dtype One of `"binary"` or `"continuous"`.
#' @param degree Polynomial degree per bin.
#' @return An `OptimalPWBinning` object.
#' @export
OptimalPWBinning <- function(name, target_dtype = "binary", degree = 1L) {
  if (!is.character(name) || length(name) != 1L || nchar(name) == 0L) {
    stop("`name` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!target_dtype %in% c("binary", "continuous")) {
    stop("`target_dtype` must be 'binary' or 'continuous'.", call. = FALSE)
  }
  if (!is.numeric(degree) || length(degree) != 1L || degree < 0) {
    stop("`degree` must be a numeric scalar >= 0.", call. = FALSE)
  }

  structure(
    list(
      name = name,
      target_dtype = target_dtype,
      degree = as.integer(degree),
      fitted = FALSE,
      base_model = NULL,
      piece_models = NULL,
      breaks = NULL,
      bin_table = NULL
    ),
    class = "OptimalPWBinning"
  )
}

#' @export
fit.OptimalPWBinning <- function(
    object,
    x,
    y,
    max_n_bins = 10L,
    algorithm = "optimal",
    prebinning_method = "cart",
    max_n_prebins = 20L,
    min_bin_size = 0.05,
    monotonic_trend = "none",
    ...
) {
  if (!is.numeric(x)) {
    stop("`x` must be numeric.", call. = FALSE)
  }
  y_num <- as.numeric(y)
  if (identical(object$target_dtype, "binary") && !all(y_num %in% c(0, 1, NA))) {
    stop("For binary piecewise binning, `y` must be in {0, 1}.", call. = FALSE)
  }
  if (identical(object$target_dtype, "continuous") && any(is.na(y_num))) {
    stop("For continuous piecewise binning, `y` must be numeric without NA.", call. = FALSE)
  }

  base <- if (identical(object$target_dtype, "binary")) {
    OptimalBinning(object$name, dtype = "numerical")
  } else {
    ContinuousOptimalBinning(object$name, dtype = "numerical")
  }
  base <- fit(
    base, x, y_num,
    max_n_bins = max_n_bins,
    algorithm = algorithm,
    prebinning_method = prebinning_method,
    max_n_prebins = max_n_prebins,
    min_bin_size = min_bin_size,
    monotonic_trend = monotonic_trend,
    ...
  )

  breaks <- base$breaks
  k <- length(breaks) - 1L
  bin_idx <- findInterval(x, breaks, rightmost.closed = TRUE)
  piece <- vector("list", k)

  for (i in seq_len(k)) {
    idx <- which(bin_idx == i & !is.na(x) & !is.na(y_num))
    if (length(idx) == 0L) {
      piece[[i]] <- NULL
      next
    }
    xi <- x[idx]
    yi <- y_num[idx]
    deg <- min(object$degree, max(0L, length(unique(xi)) - 1L))
    # Scale local x for stable polynomial fitting.
    x0 <- mean(xi)
    xs <- stats::sd(xi)
    if (!is.finite(xs) || xs == 0) xs <- 1
    zi <- (xi - x0) / xs
    df <- data.frame(y = yi, z = zi)
    form <- if (deg == 0L) y ~ 1 else stats::as.formula(sprintf("y ~ poly(z, %d, raw = TRUE)", deg))
    mod <- if (identical(object$target_dtype, "binary")) {
      # Some bins are close to complete separation; use a stable fallback.
      tryCatch(
        suppressWarnings(stats::glm(form, data = df, family = stats::binomial())),
        error = function(e) stats::lm(form, data = df)
      )
    } else {
      stats::lm(form, data = df)
    }
    piece[[i]] <- list(model = mod, center = x0, scale = xs, degree = deg)
  }

  bt <- base$bin_table
  bt$piece_degree <- vapply(piece, function(m) if (is.null(m)) NA_integer_ else m$degree, integer(1))

  object$fitted <- TRUE
  object$base_model <- base
  object$piece_models <- piece
  object$breaks <- breaks
  object$bin_table <- bt
  object
}

#' @export
transform.OptimalPWBinning <- function(object, x, ...) {
  if (!isTRUE(object$fitted)) {
    stop("Model is not fitted. Call fit() first.", call. = FALSE)
  }
  if (!is.numeric(x)) {
    stop("`x` must be numeric.", call. = FALSE)
  }
  out <- rep(NA_real_, length(x))
  k <- length(object$breaks) - 1L
  idx <- findInterval(x, object$breaks, rightmost.closed = TRUE)
  for (i in seq_len(k)) {
    ii <- which(idx == i & !is.na(x))
    if (length(ii) == 0L) next
    pm <- object$piece_models[[i]]
    if (is.null(pm)) next
    z <- (x[ii] - pm$center) / pm$scale
    if (inherits(pm$model, "glm")) {
      pred <- as.numeric(stats::predict(pm$model, newdata = data.frame(z = z), type = "response"))
    } else {
      pred <- as.numeric(stats::predict(pm$model, newdata = data.frame(z = z)))
    }
    if (identical(object$target_dtype, "binary")) {
      pred <- pmin(pmax(pred, 0), 1)
    }
    out[ii] <- pred
  }
  out
}

#' @export
print.OptimalPWBinning <- function(x, ...) {
  cat("OptimalPWBinning\n")
  cat("  Name: ", x$name, "\n", sep = "")
  cat("  Target: ", x$target_dtype, "\n", sep = "")
  cat("  Degree: ", x$degree, "\n", sep = "")
  cat("  Fitted: ", x$fitted, "\n", sep = "")
  if (isTRUE(x$fitted)) {
    cat("  Bins: ", nrow(x$bin_table), "\n", sep = "")
  }
  invisible(x)
}

#' Create an OptimalBinningSketch model object
#'
#' @param name Feature name.
#' @param sample_size Reservoir sample size.
#' @return An `OptimalBinningSketch` object.
#' @export
OptimalBinningSketch <- function(name, sample_size = 5000L) {
  if (!is.character(name) || length(name) != 1L || nchar(name) == 0L) {
    stop("`name` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.numeric(sample_size) || length(sample_size) != 1L || sample_size < 100L) {
    stop("`sample_size` must be a numeric scalar >= 100.", call. = FALSE)
  }
  structure(
    list(
      name = name,
      sample_size = as.integer(sample_size),
      n_seen = 0L,
      x_sample = numeric(0),
      y_sample = numeric(0),
      fitted = FALSE,
      base_model = NULL,
      bin_table = NULL
    ),
    class = "OptimalBinningSketch"
  )
}

#' Update sketch with a new batch
#'
#' @param object An `OptimalBinningSketch` object.
#' @param x Numeric vector.
#' @param y Binary target vector.
#' @return Updated sketch object.
#' @export
partial_fit <- function(object, x, y) {
  UseMethod("partial_fit")
}

#' @export
partial_fit.OptimalBinningSketch <- function(object, x, y) {
  if (!is.numeric(x)) stop("`x` must be numeric.", call. = FALSE)
  y_num <- as.numeric(y)
  if (!all(y_num %in% c(0, 1, NA))) stop("`y` must be binary in {0, 1}.", call. = FALSE)
  keep <- !(is.na(x) | is.na(y_num))
  x <- x[keep]
  y_num <- y_num[keep]

  for (i in seq_along(x)) {
    object$n_seen <- object$n_seen + 1L
    if (length(object$x_sample) < object$sample_size) {
      object$x_sample <- c(object$x_sample, x[i])
      object$y_sample <- c(object$y_sample, y_num[i])
    } else {
      j <- sample.int(object$n_seen, size = 1L)
      if (j <= object$sample_size) {
        object$x_sample[j] <- x[i]
        object$y_sample[j] <- y_num[i]
      }
    }
  }
  object
}

#' @export
fit.OptimalBinningSketch <- function(
    object,
    x = NULL,
    y = NULL,
    max_n_bins = 10L,
    algorithm = "optimal",
    prebinning_method = "cart",
    max_n_prebins = 20L,
    min_bin_size = 0.05,
    monotonic_trend = "none",
    ...
) {
  if (!is.null(x) && !is.null(y)) {
    object <- partial_fit(object, x, y)
  }
  if (length(object$x_sample) < 20L) {
    stop("Not enough sketch samples. Call partial_fit() with more data.", call. = FALSE)
  }
  base <- fit(
    OptimalBinning(object$name, dtype = "numerical"),
    object$x_sample,
    object$y_sample,
    max_n_bins = max_n_bins,
    algorithm = algorithm,
    prebinning_method = prebinning_method,
    max_n_prebins = max_n_prebins,
    min_bin_size = min_bin_size,
    monotonic_trend = monotonic_trend,
    ...
  )
  object$fitted <- TRUE
  object$base_model <- base
  object$bin_table <- base$bin_table
  object
}

#' @export
transform.OptimalBinningSketch <- function(object, x, ...) {
  if (!isTRUE(object$fitted)) {
    stop("Sketch model is not fitted. Call fit() first.", call. = FALSE)
  }
  transform(object$base_model, x)
}

#' @export
print.OptimalBinningSketch <- function(x, ...) {
  cat("OptimalBinningSketch\n")
  cat("  Name: ", x$name, "\n", sep = "")
  cat("  Seen: ", x$n_seen, "\n", sep = "")
  cat("  Reservoir: ", length(x$x_sample), "/", x$sample_size, "\n", sep = "")
  cat("  Fitted: ", x$fitted, "\n", sep = "")
  invisible(x)
}

#' Create an uncertainty-aware optimal binning model object
#'
#' @param name Feature name.
#' @param uncertainty_strategy One of `"mean"` or `"pessimistic"`.
#' @return An `OptimalBinningUncertainty` object.
#' @export
OptimalBinningUncertainty <- function(name, uncertainty_strategy = "mean") {
  if (!is.character(name) || length(name) != 1L || nchar(name) == 0L) {
    stop("`name` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!uncertainty_strategy %in% c("mean", "pessimistic")) {
    stop("`uncertainty_strategy` must be 'mean' or 'pessimistic'.", call. = FALSE)
  }
  structure(
    list(
      name = name,
      uncertainty_strategy = uncertainty_strategy,
      fitted = FALSE,
      base_model = NULL,
      bin_table = NULL
    ),
    class = "OptimalBinningUncertainty"
  )
}

#' @export
fit.OptimalBinningUncertainty <- function(
    object,
    x,
    y_lower,
    y_upper = NULL,
    uncertainty = NULL,
    max_n_bins = 10L,
    algorithm = "optimal",
    prebinning_method = "cart",
    max_n_prebins = 20L,
    min_bin_size = 0.05,
    monotonic_trend = "none",
    ...
) {
  if (!is.numeric(x)) stop("`x` must be numeric.", call. = FALSE)
  lo <- as.numeric(y_lower)
  hi <- if (is.null(y_upper)) lo else as.numeric(y_upper)
  if (length(lo) != length(x) || length(hi) != length(x)) {
    stop("`x`, `y_lower` and `y_upper` must have the same length.", call. = FALSE)
  }
  if (any(is.na(lo) | is.na(hi))) stop("Targets cannot include NA.", call. = FALSE)
  if (any(lo > hi)) stop("`y_lower` must be <= `y_upper` element-wise.", call. = FALSE)

  if (!is.null(uncertainty)) {
    u <- as.numeric(uncertainty)
    if (length(u) != length(x) || any(is.na(u)) || any(u < 0) || any(u > 1)) {
      stop("`uncertainty` must be numeric in [0, 1] with same length as x.", call. = FALSE)
    }
  } else {
    u <- rep(0.5, length(x))
  }

  y_prob <- if (identical(object$uncertainty_strategy, "mean")) {
    0.5 * (lo + hi)
  } else {
    (1 - u) * hi + u * lo
  }
  y_bin <- as.integer(y_prob >= 0.5)

  base <- fit(
    OptimalBinning(object$name, dtype = "numerical"),
    x,
    y_bin,
    max_n_bins = max_n_bins,
    algorithm = algorithm,
    prebinning_method = prebinning_method,
    max_n_prebins = max_n_prebins,
    min_bin_size = min_bin_size,
    monotonic_trend = monotonic_trend,
    ...
  )
  object$fitted <- TRUE
  object$base_model <- base
  object$bin_table <- base$bin_table
  object
}

#' @export
transform.OptimalBinningUncertainty <- function(object, x, ...) {
  if (!isTRUE(object$fitted)) {
    stop("Uncertainty model is not fitted. Call fit() first.", call. = FALSE)
  }
  transform(object$base_model, x)
}

#' @export
print.OptimalBinningUncertainty <- function(x, ...) {
  cat("OptimalBinningUncertainty\n")
  cat("  Name: ", x$name, "\n", sep = "")
  cat("  Strategy: ", x$uncertainty_strategy, "\n", sep = "")
  cat("  Fitted: ", x$fitted, "\n", sep = "")
  invisible(x)
}
