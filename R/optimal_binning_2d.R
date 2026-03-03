#' Create an OptimalBinning2D model object
#'
#' @param name_x First feature name.
#' @param name_y Second feature name.
#' @param target_dtype One of `"binary"` or `"continuous"`.
#' @return An `OptimalBinning2D` object.
#' @export
OptimalBinning2D <- function(name_x, name_y, target_dtype = "binary") {
  if (!is.character(name_x) || length(name_x) != 1L || nchar(name_x) == 0L) {
    stop("`name_x` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.character(name_y) || length(name_y) != 1L || nchar(name_y) == 0L) {
    stop("`name_y` must be a non-empty character scalar.", call. = FALSE)
  }
  if (!target_dtype %in% c("binary", "continuous")) {
    stop("`target_dtype` must be 'binary' or 'continuous'.", call. = FALSE)
  }

  structure(
    list(
      name_x = name_x,
      name_y = name_y,
      target_dtype = target_dtype,
      fitted = FALSE,
      breaks_x = NULL,
      breaks_y = NULL,
      bin_table = NULL,
      n_bins = NULL,
      total_iv = NULL,
      fit_params = NULL,
      bin_map = NULL
    ),
    class = "OptimalBinning2D"
  )
}

.unique_quantile_breaks <- function(x, n_bins) {
  probs <- seq(0, 1, length.out = n_bins + 1L)
  qs <- as.numeric(stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 8))
  br <- unique(qs)
  if (length(br) < 2L) {
    rng <- range(x, na.rm = TRUE)
    if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
      return(c(-Inf, Inf))
    }
    br <- c(rng[1], rng[2])
  }
  br[1] <- -Inf
  br[length(br)] <- Inf
  br
}

.interval_label <- function(lo, hi) {
  paste0("(", format(lo, trim = TRUE), ",", format(hi, trim = TRUE), "]")
}

#' @export
fit.OptimalBinning2D <- function(
    object,
    x,
    y,
    max_n_bins_x = 8L,
    max_n_bins_y = 8L,
    min_bin_size = 0.01,
    eps = 1e-6,
    ...
) {
  if (!is.data.frame(x) || ncol(x) != 2L) {
    stop("For OptimalBinning2D, `x` must be a data.frame with exactly 2 numeric columns.", call. = FALSE)
  }
  x1 <- x[[1]]
  x2 <- x[[2]]
  if (!is.numeric(x1) || !is.numeric(x2)) {
    stop("Both 2D predictors must be numeric.", call. = FALSE)
  }
  if (length(y) != nrow(x)) {
    stop("`x` and `y` must have compatible lengths.", call. = FALSE)
  }
  if (!is.numeric(max_n_bins_x) || length(max_n_bins_x) != 1L || max_n_bins_x < 2L) {
    stop("`max_n_bins_x` must be numeric scalar >= 2.", call. = FALSE)
  }
  if (!is.numeric(max_n_bins_y) || length(max_n_bins_y) != 1L || max_n_bins_y < 2L) {
    stop("`max_n_bins_y` must be numeric scalar >= 2.", call. = FALSE)
  }
  if (!is.numeric(min_bin_size) || length(min_bin_size) != 1L || min_bin_size <= 0 || min_bin_size > 1) {
    stop("`min_bin_size` must be in (0, 1].", call. = FALSE)
  }

  y_num <- as.numeric(y)
  if (identical(object$target_dtype, "binary")) {
    if (!all(y_num %in% c(0, 1, NA))) {
      stop("For binary 2D binning, `y` must be in {0, 1}.", call. = FALSE)
    }
  } else {
    if (any(is.na(y_num))) {
      stop("For continuous 2D binning, `y` must be numeric without NA.", call. = FALSE)
    }
  }

  keep <- !(is.na(x1) | is.na(x2) | is.na(y_num))
  x1 <- x1[keep]
  x2 <- x2[keep]
  y_num <- y_num[keep]
  n <- length(y_num)
  if (n < 10L) {
    stop("Not enough complete observations for 2D binning.", call. = FALSE)
  }

  bx <- .unique_quantile_breaks(x1, as.integer(max_n_bins_x))
  by <- .unique_quantile_breaks(x2, as.integer(max_n_bins_y))
  ix <- findInterval(x1, bx, rightmost.closed = TRUE)
  iy <- findInterval(x2, by, rightmost.closed = TRUE)
  nx <- length(bx) - 1L
  ny <- length(by) - 1L
  valid <- ix >= 1L & ix <= nx & iy >= 1L & iy <= ny
  ix <- ix[valid]
  iy <- iy[valid]
  y_num <- y_num[valid]
  n <- length(y_num)

  cell_id <- paste(ix, iy, sep = "_")
  count_tab <- tapply(y_num, cell_id, length)
  keys <- names(count_tab)
  count <- as.numeric(count_tab)
  min_count <- max(1L, floor(n * min_bin_size))
  keep_cell <- count >= min_count
  keys <- keys[keep_cell]
  count <- count[keep_cell]
  if (length(keys) == 0L) {
    stop("No 2D bins satisfy `min_bin_size`.", call. = FALSE)
  }

  key_ix <- as.integer(vapply(strsplit(keys, "_", fixed = TRUE), `[`, character(1), 1))
  key_iy <- as.integer(vapply(strsplit(keys, "_", fixed = TRUE), `[`, character(1), 2))
  lx <- mapply(.interval_label, bx[key_ix], bx[key_ix + 1L], USE.NAMES = FALSE)
  ly <- mapply(.interval_label, by[key_iy], by[key_iy + 1L], USE.NAMES = FALSE)
  bin_lbl <- paste0("x:", lx, " | y:", ly)

  if (identical(object$target_dtype, "binary")) {
    event <- as.numeric(tapply(y_num == 1, cell_id, sum))[keep_cell]
    non_event <- as.numeric(tapply(y_num == 0, cell_id, sum))[keep_cell]
    te <- sum(event)
    tn <- sum(non_event)
    if (te == 0L || tn == 0L) {
      stop("`y` must include both classes 0 and 1 for binary 2D binning.", call. = FALSE)
    }
    dist_event <- pmax(event / te, eps)
    dist_non_event <- pmax(non_event / tn, eps)
    woe <- log(dist_non_event / dist_event)
    iv <- (dist_non_event - dist_event) * woe

    tab <- data.frame(
      bin = bin_lbl,
      count = count,
      event = event,
      non_event = non_event,
      event_rate = event / pmax(count, 1),
      woe = woe,
      iv = iv,
      ix = key_ix,
      iy = key_iy,
      stringsAsFactors = FALSE
    )
    map_vals <- stats::setNames(tab$woe, paste(tab$ix, tab$iy, sep = "_"))
    total_iv <- sum(iv, na.rm = TRUE)
  } else {
    mu <- as.numeric(tapply(y_num, cell_id, mean))[keep_cell]
    tab <- data.frame(
      bin = bin_lbl,
      count = count,
      event = NA_real_,
      non_event = NA_real_,
      event_rate = mu,
      woe = NA_real_,
      iv = NA_real_,
      ix = key_ix,
      iy = key_iy,
      stringsAsFactors = FALSE
    )
    map_vals <- stats::setNames(tab$event_rate, paste(tab$ix, tab$iy, sep = "_"))
    total_iv <- NA_real_
  }

  object$fitted <- TRUE
  object$breaks_x <- bx
  object$breaks_y <- by
  object$bin_table <- tab
  object$n_bins <- nrow(tab)
  object$total_iv <- total_iv
  object$fit_params <- list(
    max_n_bins_x = as.integer(max_n_bins_x),
    max_n_bins_y = as.integer(max_n_bins_y),
    min_bin_size = min_bin_size
  )
  object$bin_map <- map_vals
  object
}

#' @export
transform.OptimalBinning2D <- function(object, x, ...) {
  if (!isTRUE(object$fitted)) {
    stop("Model is not fitted. Call fit() first.", call. = FALSE)
  }
  if (!is.data.frame(x) || ncol(x) != 2L) {
    stop("For OptimalBinning2D, `x` must be a data.frame with exactly 2 columns.", call. = FALSE)
  }
  x1 <- x[[1]]
  x2 <- x[[2]]
  if (!is.numeric(x1) || !is.numeric(x2)) {
    stop("Both 2D predictors must be numeric.", call. = FALSE)
  }
  out <- rep(NA_real_, nrow(x))
  keep <- !(is.na(x1) | is.na(x2))
  if (any(keep)) {
    ix <- findInterval(x1[keep], object$breaks_x, rightmost.closed = TRUE)
    iy <- findInterval(x2[keep], object$breaks_y, rightmost.closed = TRUE)
    key <- paste(ix, iy, sep = "_")
    out[keep] <- as.numeric(object$bin_map[key])
  }
  out
}

#' @export
print.OptimalBinning2D <- function(x, ...) {
  cat("OptimalBinning2D\n")
  cat("  Name x: ", x$name_x, "\n", sep = "")
  cat("  Name y: ", x$name_y, "\n", sep = "")
  cat("  Target: ", x$target_dtype, "\n", sep = "")
  cat("  Fitted: ", x$fitted, "\n", sep = "")
  if (isTRUE(x$fitted)) {
    cat("  Bins:   ", x$n_bins, "\n", sep = "")
  }
  invisible(x)
}
