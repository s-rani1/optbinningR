#' Get binning table handle
#'
#' @param object A fitted binning model object.
#' @return A `BinningTable` object. Use `build()` to materialize a Python-style table.
#' @export
binning_table <- function(object) {
  supported <- inherits(object, "OptimalBinning") ||
    inherits(object, "OptimalBinning2D") ||
    inherits(object, "OptimalPWBinning") ||
    inherits(object, "OptimalBinningSketch") ||
    inherits(object, "OptimalBinningUncertainty")
  if (!supported) {
    stop("`object` must be a supported binning model instance.", call. = FALSE)
  }
  if (!isTRUE(object$fitted)) {
    stop("Model is not fitted. Call fit() first.", call. = FALSE)
  }
  structure(list(model = object), class = "BinningTable")
}

#' Build a Python-style binning table
#'
#' @param object A `BinningTable` object.
#' @param ... Reserved for compatibility.
#' @return A data.frame matching Python-style binning table columns.
#' @export
build <- function(object, ...) {
  UseMethod("build")
}

.bin_rows_with_special_missing <- function(bt) {
  core <- bt[!(bt$bin %in% c("Special", "Missing")), , drop = FALSE]
  special <- bt[bt$bin == "Special", , drop = FALSE]
  missing <- bt[bt$bin == "Missing", , drop = FALSE]
  if (nrow(special) == 0L) {
    special <- data.frame(bin = "Special", count = 0, event = 0, non_event = 0, event_rate = 0, woe = 0, iv = 0, stringsAsFactors = FALSE)
  }
  if (nrow(missing) == 0L) {
    missing <- data.frame(bin = "Missing", count = 0, event = 0, non_event = 0, event_rate = 0, woe = 0, iv = 0, stringsAsFactors = FALSE)
  }
  rbind(core, special[1, , drop = FALSE], missing[1, , drop = FALSE])
}

.python_interval_labels_from_splits <- function(splits, digits = 2L) {
  fmt <- function(v) format(round(as.numeric(v), digits = digits), trim = TRUE, nsmall = digits)
  s <- as.numeric(splits)
  if (length(s) == 0L) {
    return(character(0))
  }
  out <- character(length(s) + 1L)
  out[1L] <- paste0("(-inf, ", fmt(s[1L]), ")")
  if (length(s) > 1L) {
    for (i in 2:length(s)) {
      out[i] <- paste0("[", fmt(s[i - 1L]), ", ", fmt(s[i]), ")")
    }
  }
  out[length(out)] <- paste0("[", fmt(s[length(s)]), ", inf)")
  out
}

.js_contrib <- function(event, non_event) {
  te <- sum(event)
  tn <- sum(non_event)
  if (te <= 0 || tn <= 0) {
    return(rep(0, length(event)))
  }
  p <- event / te
  q <- non_event / tn
  m <- 0.5 * (p + q)
  term_p <- ifelse(p > 0, p * log(p / m), 0)
  term_q <- ifelse(q > 0, q * log(q / m), 0)
  0.5 * (term_p + term_q)
}

.build_binary_table <- function(model) {
  rows <- .bin_rows_with_special_missing(model$bin_table)
  core_n <- sum(!(rows$bin %in% c("Special", "Missing")))
  if (identical(model$dtype, "numerical") && core_n > 1L && is.numeric(model$splits) && length(model$splits) == core_n - 1L) {
    labels <- .python_interval_labels_from_splits(model$splits, digits = 2L)
    if (length(labels) == core_n) {
      rows$bin[seq_len(core_n)] <- labels
    }
  }
  n <- sum(rows$count)
  out <- data.frame(
    Bin = rows$bin,
    Count = as.numeric(rows$count),
    `Count (%)` = if (n > 0) as.numeric(rows$count) / n else 0,
    `Non-event` = as.numeric(rows$non_event),
    Event = as.numeric(rows$event),
    `Event rate` = ifelse(rows$count > 0, as.numeric(rows$event) / as.numeric(rows$count), 0),
    WoE = as.numeric(rows$woe),
    IV = as.numeric(rows$iv),
    JS = .js_contrib(as.numeric(rows$event), as.numeric(rows$non_event)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  totals <- data.frame(
    Bin = "",
    Count = sum(out$Count, na.rm = TRUE),
    `Count (%)` = if (sum(out$Count, na.rm = TRUE) > 0) 1 else 0,
    `Non-event` = sum(out$`Non-event`, na.rm = TRUE),
    Event = sum(out$Event, na.rm = TRUE),
    `Event rate` = if (sum(out$Count, na.rm = TRUE) > 0) sum(out$Event, na.rm = TRUE) / sum(out$Count, na.rm = TRUE) else 0,
    WoE = sum(abs(out$WoE), na.rm = TRUE),
    IV = sum(out$IV, na.rm = TRUE),
    JS = sum(out$JS, na.rm = TRUE),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  out <- rbind(out, totals)
  rn <- c(as.character(seq_len(nrow(out) - 1L) - 1L), "Totals")
  rownames(out) <- rn
  out
}

.build_continuous_table <- function(model) {
  fit_data <- model$fit_data
  if (is.null(fit_data) || is.null(fit_data$x) || is.null(fit_data$y)) {
    stop("Continuous table build requires fit data. Refit model with current package version.", call. = FALSE)
  }
  x <- fit_data$x
  y <- fit_data$y
  is_special <- fit_data$is_special
  is_missing <- fit_data$is_missing
  is_clean <- !(is_special | is_missing)

  breaks <- model$breaks
  core_labels <- model$bin_table$bin
  core_labels <- core_labels[!(core_labels %in% c("Special", "Missing"))]
  if (length(core_labels) > 1L && is.numeric(model$splits) && length(model$splits) == length(core_labels) - 1L) {
    labels <- .python_interval_labels_from_splits(model$splits, digits = 2L)
    if (length(labels) == length(core_labels)) {
      core_labels <- labels
    }
  }

  bins <- cut(x[is_clean], breaks = breaks, include.lowest = TRUE, right = TRUE)
  lv <- levels(bins)

  core_rows <- lapply(seq_along(lv), function(i) {
    mask <- bins == lv[i]
    yi <- y[is_clean][mask]
    data.frame(
      Bin = core_labels[i],
      Count = length(yi),
      Sum = sum(yi),
      Std = pop_sd(yi),
      Mean = if (length(yi) > 0L) mean(yi) else 0,
      Min = if (length(yi) > 0L) min(yi) else NaN,
      Max = if (length(yi) > 0L) max(yi) else NaN,
      `Zeros count` = sum(yi == 0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, core_rows)

  add_special <- function(label, mask) {
    yi <- y[mask]
    data.frame(
      Bin = label,
      Count = length(yi),
      Sum = sum(yi),
      Std = pop_sd(yi),
      Mean = if (length(yi) > 0L) mean(yi) else 0,
      Min = if (length(yi) > 0L) min(yi) else NaN,
      Max = if (length(yi) > 0L) max(yi) else NaN,
      `Zeros count` = sum(yi == 0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }

  out <- rbind(out, add_special("Special", is_special), add_special("Missing", is_missing))

  n <- sum(out$Count)
  out$`Count (%)` <- if (n > 0) out$Count / n else 0
  total_mean <- if (sum(out$Count) > 0) sum(out$Sum) / sum(out$Count) else 0
  out$WoE <- out$Mean - total_mean
  out$IV <- abs(out$WoE) * out$`Count (%)`

  out <- out[, c("Bin", "Count", "Count (%)", "Sum", "Std", "Mean", "Min", "Max", "Zeros count", "WoE", "IV")]

  totals <- data.frame(
    Bin = "",
    Count = sum(out$Count, na.rm = TRUE),
    `Count (%)` = if (sum(out$Count, na.rm = TRUE) > 0) 1 else 0,
    Sum = sum(out$Sum, na.rm = TRUE),
    Std = NaN,
    Mean = total_mean,
    Min = min(out$Min[is.finite(out$Min)], na.rm = TRUE),
    Max = max(out$Max[is.finite(out$Max)], na.rm = TRUE),
    `Zeros count` = sum(out$`Zeros count`, na.rm = TRUE),
    WoE = sum(abs(out$WoE), na.rm = TRUE),
    IV = sum(out$IV, na.rm = TRUE),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  out <- rbind(out, totals)
  rn <- c(as.character(seq_len(nrow(out) - 1L) - 1L), "Totals")
  rownames(out) <- rn
  out
}

.build_multiclass_table <- function(model) {
  bt <- model$bin_table
  class_count_cols <- grep("^event_class_", names(bt), value = TRUE)
  class_rate_cols <- sub("^event_class_", "event_rate_class_", class_count_cols)

  core <- bt[!(bt$bin %in% c("Special", "Missing")), , drop = FALSE]
  if (nrow(core) > 1L && is.numeric(model$splits) && length(model$splits) == nrow(core) - 1L) {
    labels <- .python_interval_labels_from_splits(model$splits, digits = 2L)
    if (length(labels) == nrow(core)) {
      core$bin <- labels
    }
  }
  n <- sum(core$count)

  out <- data.frame(
    Bin = core$bin,
    Count = as.numeric(core$count),
    `Count (%)` = if (n > 0) as.numeric(core$count) / n else 0,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  for (i in seq_along(class_count_cols)) {
    ec <- class_count_cols[i]
    er <- class_rate_cols[i]
    cl <- sub("^event_class_", "", ec)
    out[[paste0("Event_", cl)]] <- if (ec %in% names(core)) as.numeric(core[[ec]]) else 0
    out[[paste0("Event_rate_", cl)]] <- if (er %in% names(core)) as.numeric(core[[er]]) else 0
  }

  totals <- out[1, , drop = FALSE]
  totals[1, ] <- NA
  totals$Bin <- ""
  totals$Count <- sum(out$Count, na.rm = TRUE)
  totals$`Count (%)` <- if (sum(out$Count, na.rm = TRUE) > 0) 1 else 0
  for (nm in names(out)) {
    if (startsWith(nm, "Event_")) {
      totals[[nm]] <- sum(out[[nm]], na.rm = TRUE)
    }
    if (startsWith(nm, "Event_rate_")) {
      totals[[nm]] <- NA_real_
    }
  }

  out <- rbind(out, totals)
  rn <- c(as.character(seq_len(nrow(out) - 1L) - 1L), "Totals")
  rownames(out) <- rn
  out
}

#' @export
build.BinningTable <- function(object, ...) {
  model <- object$model
  if (inherits(model, "OptimalBinning2D")) {
    return(model$bin_table)
  }
  if (identical(model$target_dtype, "binary")) {
    return(.build_binary_table(model))
  }
  if (identical(model$target_dtype, "continuous")) {
    return(.build_continuous_table(model))
  }
  if (identical(model$target_dtype, "multiclass")) {
    return(.build_multiclass_table(model))
  }
  model$bin_table
}

.multiclass_monotonic <- function(v) {
  if (length(v) <= 1L || all(!is.finite(v))) return("none")
  if (all(diff(v) >= -1e-12)) return("ascending")
  if (all(diff(v) <= 1e-12)) return("descending")
  n <- length(v)
  for (k in seq_len(n)) {
    if (all(diff(v[seq_len(k)]) >= -1e-12) && all(diff(v[k:n]) <= 1e-12)) return("peak")
    if (all(diff(v[seq_len(k)]) <= 1e-12) && all(diff(v[k:n]) >= -1e-12)) return("valley")
  }
  "none"
}

#' @export
analysis.BinningTable <- function(object) {
  model <- object$model
  if (!identical(model$target_dtype, "multiclass")) {
    return(analysis(model))
  }

  bt <- build(object)
  core <- bt[rownames(bt) != "Totals", , drop = FALSE]

  event_cols <- grep("^Event_(?!rate_)", names(core), value = TRUE, perl = TRUE)
  event_mat <- as.matrix(core[, event_cols, drop = FALSE])
  event_mat[is.na(event_mat)] <- 0
  n <- sum(event_mat)
  row_sum <- rowSums(event_mat)
  col_sum <- colSums(event_mat)

  p_global <- if (sum(col_sum) > 0) col_sum / sum(col_sum) else rep(0, length(col_sum))
  js_rows <- numeric(nrow(event_mat))
  for (i in seq_len(nrow(event_mat))) {
    if (row_sum[i] <= 0) next
    p <- event_mat[i, ] / row_sum[i]
    m <- 0.5 * (p + p_global)
    t1 <- ifelse(p > 0, p * log(p / m), 0)
    t2 <- ifelse(p_global > 0, p_global * log(p_global / m), 0)
    js_rows[i] <- 0.5 * sum(t1 + t2)
  }
  js <- if (n > 0) sum((row_sum / n) * js_rows) else 0

  hhi <- if (n > 0) sum((row_sum / n)^2) else 0
  k <- max(1L, nrow(event_mat))
  hhi_norm <- if (k > 1L) (hhi - 1 / k) / (1 - 1 / k) else 0

  if (n > 0 && nrow(event_mat) > 0 && ncol(event_mat) > 0) {
    chis <- suppressWarnings(stats::chisq.test(event_mat, correct = FALSE))
    chis_stat <- as.numeric(chis$statistic)
  } else {
    chis <- list(statistic = 0, p.value = NA_real_)
    chis_stat <- 0
  }
  r <- nrow(event_mat)
  c <- ncol(event_mat)
  cramers_v <- if (n > 0 && min(r - 1L, c - 1L) > 0L) sqrt(chis_stat / (n * min(r - 1L, c - 1L))) else 0

  quality <- js * (1 - hhi_norm) * cramers_v

  trends <- vapply(seq_along(event_cols), function(j) {
    rate_col <- sub("^Event_", "Event_rate_", event_cols[j])
    .multiclass_monotonic(as.numeric(core[[rate_col]]))
  }, character(1))
  names(trends) <- sub("^Event_", "Class ", event_cols)

  sig <- data.frame(`Bin A` = integer(0), `Bin B` = integer(0), `t-statistic` = numeric(0), `p-value` = numeric(0), check.names = FALSE)
  if (nrow(event_mat) > 1L) {
    for (i in seq_len(nrow(event_mat) - 1L)) {
      pair <- rbind(event_mat[i, ], event_mat[i + 1L, ])
      if (sum(pair) > 0) {
        tst <- suppressWarnings(stats::chisq.test(pair, correct = FALSE))
        tstat <- sqrt(as.numeric(tst$statistic))
        pval <- as.numeric(tst$p.value)
      } else {
        tstat <- 0
        pval <- NA_real_
      }
      sig <- rbind(sig, data.frame(`Bin A` = i - 1L, `Bin B` = i, `t-statistic` = tstat, `p-value` = pval, check.names = FALSE))
    }
  }

  cat("---------------------------------------------\n")
  cat("OptimalBinning: Multiclass Binning Table Analysis\n")
  cat("---------------------------------------------\n\n")
  cat("  General metrics\n\n")
  cat(sprintf("    JS (Jensen-Shannon)    %.8f\n", js))
  cat(sprintf("    HHI                    %.8f\n", hhi))
  cat(sprintf("    HHI (normalized)       %.8f\n", hhi_norm))
  cat(sprintf("    Cramer's V             %.8f\n", cramers_v))
  cat(sprintf("    Quality score          %.8f\n\n", quality))
  cat("  Monotonic trend\n\n")
  for (nm in names(trends)) {
    cat(sprintf("    %-8s              %s\n", nm, trends[[nm]]))
  }
  cat("\n  Significance tests\n\n")
  print(sig, row.names = FALSE)

  invisible(list(
    js = js,
    hhi = hhi,
    hhi_norm = hhi_norm,
    cramers_v = cramers_v,
    quality_score = quality,
    monotonic_trend = trends,
    significance_tests = sig
  ))
}

#' @export
print.BinningTable <- function(x, ...) {
  cat("The binning_table is instantiated, but not built.\n")
  cat("Call `build(binning_table_object)` to get a data.frame.\n")
  invisible(x)
}
  pop_sd <- function(v) {
    if (length(v) == 0L) return(NaN)
    m <- mean(v)
    sqrt(mean((v - m)^2))
  }
