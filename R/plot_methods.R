#' Plot binning diagnostics
#'
#' @param x A fitted binning object.
#' @param metric Metric to draw on the secondary axis. Supported values are
#'   `"woe"`, `"event_rate"`, `"iv"` and `"mean"` (continuous target).
#' @param add_special Whether to include the special bin in the plot.
#' @param add_missing Whether to include the missing bin in the plot.
#' @param style Plot style. `"bin"` uses bin index on x-axis. `"actual"`
#'   uses actual numerical bin widths.
#' @param show_bin_labels Whether to show bin labels instead of bin ids. Only
#'   supported with `style = "bin"`.
#' @param main Optional main title.
#' @param xlab Optional x-axis label.
#' @param ylab Optional primary y-axis label.
#' @param type Deprecated alias for compatibility (`"binning"` ->
#'   `metric = "event_rate"`, `"woe"` -> `metric = "woe"`).
#' @param ... Additional graphical parameters.
#' @export
.plot_continuous_optbinning <- function(
    x, bt, metric, add_special, add_missing, style, show_bin_labels, main, xlab, ylab, ...
) {
  style <- match.arg(style, c("bin", "actual"))
  metric <- if (identical(metric, "event_rate")) "mean" else metric
  metric <- match.arg(metric, c("mean", "iv", "woe"))
  metric_col <- switch(metric, mean = "event_rate", iv = "iv", woe = "woe")
  metric_label <- switch(metric, mean = "Mean", iv = "IV", woe = "WoE")

  is_special <- bt$bin %in% "Special"
  is_missing <- bt$bin %in% "Missing"
  is_core <- !(is_special | is_missing)

  if (identical(style, "actual")) {
    if (!identical(x$dtype, "numerical")) {
      stop("If style = 'actual', dtype must be numerical.", call. = FALSE)
    }
    core <- bt[is_core, , drop = FALSE]
    breaks <- x$breaks
    if (!is.numeric(breaks) || length(breaks) != (nrow(core) + 1L) || any(!is.finite(breaks))) {
      stop("`style = 'actual'` requires finite numerical breaks.", call. = FALSE)
    }
    left <- breaks[-length(breaks)]
    right <- breaks[-1L]
    mid <- (left + right) / 2
    main_use <- if (is.null(main)) x$name else main
    xlab_use <- if (is.null(xlab)) "x" else xlab
    ylab_use <- if (is.null(ylab)) "Bin count" else ylab
    graphics::plot(c(min(left), max(right)), c(0, max(core$count, na.rm = TRUE) * 1.1),
      type = "n", xlab = xlab_use, ylab = ylab_use, main = main_use, ...
    )
    for (i in seq_len(nrow(core))) {
      graphics::rect(left[i], 0, right[i], core$count[i], col = "#1f77b4", border = NA)
    }
    mv <- core[[metric_col]]
    graphics::par(new = TRUE)
    yr <- range(mv, na.rm = TRUE)
    if (!all(is.finite(yr))) yr <- c(0, 1)
    if (abs(diff(yr)) < .Machine$double.eps) yr <- yr + c(-0.5, 0.5)
    graphics::plot(c(min(left), max(right)), yr, type = "n", axes = FALSE, xlab = "", ylab = "")
    for (i in seq_len(nrow(core))) graphics::lines(c(left[i], right[i]), rep(mv[i], 2L), col = "black")
    graphics::points(mid, mv, pch = 16, col = "black")
    if (length(breaks) > 2L) graphics::abline(v = breaks[2:(length(breaks) - 1L)], lty = 2, col = "black")
    graphics::axis(4)
    graphics::mtext(metric_label, side = 4, line = 2)
    graphics::box(bty = "o")
    graphics::legend("bottom", legend = "Count", fill = "#1f77b4", ncol = 1, bty = "n", inset = c(0, -0.2), xpd = TRUE)
    return(invisible(x))
  }

  keep <- is_core | (add_special & is_special) | (add_missing & is_missing)
  tbl <- bt[keep, , drop = FALSE]
  ids <- seq_len(nrow(tbl)) - 1L
  labels <- if (show_bin_labels) as.character(tbl$bin) else as.character(ids)
  main_use <- if (is.null(main)) x$name else main
  xlab_use <- if (is.null(xlab)) if (show_bin_labels) "Bin" else "Bin ID" else xlab
  ylab_use <- if (is.null(ylab)) "Bin count" else ylab
  las_use <- if (show_bin_labels) 2 else 1
  centers <- graphics::barplot(tbl$count, col = "#1f77b4", border = NA, names.arg = labels,
    xlab = xlab_use, ylab = ylab_use, ylim = c(0, max(tbl$count, na.rm = TRUE) * 1.1),
    main = main_use, las = las_use, ...
  )
  mv <- tbl[[metric_col]]
  core_idx <- which(!(tbl$bin %in% c("Special", "Missing")))
  graphics::par(new = TRUE)
  yr <- range(mv, na.rm = TRUE)
  if (!all(is.finite(yr))) yr <- c(0, 1)
  if (abs(diff(yr)) < .Machine$double.eps) yr <- yr + c(-0.5, 0.5)
  graphics::plot(centers, mv, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = range(centers) + c(-0.6, 0.6), ylim = yr)
  if (length(core_idx) > 0L) graphics::lines(centers[core_idx], mv[core_idx], type = "b", pch = 16, col = "black")
  side_idx <- setdiff(seq_len(nrow(tbl)), core_idx)
  if (length(side_idx) > 0L) graphics::points(centers[side_idx], mv[side_idx], pch = 16, col = "black")
  graphics::axis(4)
  graphics::mtext(metric_label, side = 4, line = 2)
  graphics::box(bty = "o")

  legend_labels <- "Count"
  legend_fill <- "#1f77b4"
  if (any(tbl$bin %in% "Special")) {
    legend_labels <- c(legend_labels, "Bin special")
    legend_fill <- c(legend_fill, "gray80")
  }
  if (any(tbl$bin %in% "Missing")) {
    legend_labels <- c(legend_labels, "Bin missing")
    legend_fill <- c(legend_fill, "gray60")
  }
  usr <- graphics::par("usr")
  leg_x <- mean(usr[1:2])
  leg_y <- usr[3] - 0.20 * (usr[4] - usr[3])
  graphics::legend(
    x = leg_x, y = leg_y,
    legend = legend_labels,
    fill = legend_fill,
    border = "black",
    ncol = 2,
    bty = "o",
    bg = "gray95",
    box.col = "gray70",
    xjust = 0.5,
    yjust = 1,
    xpd = NA,
    cex = 0.82,
    pt.cex = 1.2,
    x.intersp = 1.2,
    y.intersp = 1.1
  )
  invisible(x)
}

.plot_multiclass_optbinning <- function(
    x, bt, metric, add_special, add_missing, style, show_bin_labels, main, xlab, ylab, ...
) {
  op_local <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op_local), add = TRUE)
  mar_local <- graphics::par("mar")
  mar_local[1] <- max(mar_local[1], 10.5)
  if (isTRUE(show_bin_labels)) {
    mar_local[1] <- max(mar_local[1], 15.5)
  }
  graphics::par(mar = mar_local)

  if (!identical(style, "bin")) {
    stop("Multiclass plot supports style = 'bin' only.", call. = FALSE)
  }
  metric <- match.arg(metric, c("event_rate"))

  is_special <- bt$bin %in% "Special"
  is_missing <- bt$bin %in% "Missing"
  is_core <- !(is_special | is_missing)
  keep <- is_core | (add_special & is_special) | (add_missing & is_missing)
  tbl <- bt[keep, , drop = FALSE]

  ev_cols <- grep("^event_class_", names(tbl), value = TRUE)
  rate_cols <- grep("^event_rate_class_", names(tbl), value = TRUE)

  if (length(ev_cols) == 0L || length(rate_cols) == 0L) {
    # Fallback if older models don't carry per-class details.
    return(.plot_continuous_optbinning(
      x = x, bt = tbl, metric = "mean",
      add_special = add_special, add_missing = add_missing,
      style = "bin", show_bin_labels = show_bin_labels, main = main, xlab = xlab, ylab = ylab, ...
    ))
  }

  ids <- seq_len(nrow(tbl)) - 1L
  labels <- as.character(ids)
  if (show_bin_labels) {
    if (identical(x$dtype, "numerical") && is.numeric(x$breaks) && length(x$breaks) >= 2L) {
      core_n <- sum(!(tbl$bin %in% c("Special", "Missing")))
      if (length(x$breaks) == (core_n + 1L)) {
        b <- x$breaks
        fmt <- function(v) format(round(v, 2), trim = TRUE, nsmall = 2)
        core_labels <- character(core_n)
        core_labels[1] <- paste0("(-inf,", fmt(b[2]), "]")
        if (core_n > 2L) {
          for (i in 2:(core_n - 1L)) {
            core_labels[i] <- paste0("(", fmt(b[i]), ",", fmt(b[i + 1L]), "]")
          }
        }
        if (core_n >= 2L) {
          core_labels[core_n] <- paste0("(", fmt(b[core_n]), ",inf)")
        }
        labels <- core_labels
        if (any(tbl$bin %in% "Special")) labels <- c(labels, "Special")
        if (any(tbl$bin %in% "Missing")) labels <- c(labels, "Missing")
      } else {
        labels <- as.character(tbl$bin)
      }
    } else {
      labels <- as.character(tbl$bin)
    }
  }
  main_use <- if (is.null(main)) x$name else main
  xlab_use <- if (is.null(xlab)) if (show_bin_labels) "Bin" else "Bin ID" else xlab
  ylab_use <- if (is.null(ylab)) "Bin count" else ylab

  counts_mat <- as.matrix(tbl[, ev_cols, drop = FALSE])
  storage.mode(counts_mat) <- "double"
  rates_mat <- as.matrix(tbl[, rate_cols, drop = FALSE])
  storage.mode(rates_mat) <- "double"
  class_labels <- if (!is.null(x$classes) && length(x$classes) == ncol(counts_mat)) as.character(x$classes) else gsub("^event_class_", "", ev_cols)
  # Match tutorial palette: red, purple, green for classes 0/1/2.
  base_palette <- c("#e31a1c", "#8e44ad", "#1b9e00")
  if (ncol(counts_mat) <= length(base_palette)) {
    class_colors <- base_palette[seq_len(ncol(counts_mat))]
  } else {
    class_colors <- c(base_palette, grDevices::hcl.colors(ncol(counts_mat) - length(base_palette), palette = "Set 2"))
  }

  if (show_bin_labels) {
    centers <- graphics::barplot(
      t(counts_mat), col = class_colors, border = NA, names.arg = rep("", length(labels)),
      xlab = xlab_use, ylab = ylab_use, ylim = c(0, max(colSums(counts_mat), na.rm = TRUE) * 1.1),
      xaxt = "n",
      main = main_use, ...
    )
    usr0 <- graphics::par("usr")
    y_lab <- usr0[3] - 0.02 * (usr0[4] - usr0[3])
    graphics::text(centers, y_lab, labels = labels, srt = 45, adj = 1, xpd = NA, cex = 0.8)
  } else {
    centers <- graphics::barplot(
      t(counts_mat), col = class_colors, border = NA, names.arg = labels,
      xlab = xlab_use, ylab = ylab_use, ylim = c(0, max(colSums(counts_mat), na.rm = TRUE) * 1.1),
      las = 1, cex.names = 1,
      main = main_use, ...
    )
  }

  core_idx <- which(!(tbl$bin %in% c("Special", "Missing")))
  graphics::par(new = TRUE)
  yr <- range(rates_mat, na.rm = TRUE)
  if (!all(is.finite(yr))) yr <- c(0, 1)
  if (abs(diff(yr)) < .Machine$double.eps) yr <- yr + c(-0.5, 0.5)
  graphics::plot(centers, rates_mat[, 1], type = "n", axes = FALSE, xlab = "", ylab = "", xlim = range(centers) + c(-0.6, 0.6), ylim = yr)
  for (j in seq_len(ncol(rates_mat))) {
    if (length(core_idx) > 0L) {
      graphics::lines(centers[core_idx], rates_mat[core_idx, j], col = "black")
      graphics::points(centers[core_idx], rates_mat[core_idx, j], pch = 21, bg = class_colors[j], col = "black")
    }
  }
  graphics::axis(4)
  graphics::mtext("Event rate", side = 4, line = 2)
  graphics::box(bty = "o")

  legend_labels <- class_labels
  legend_fill <- class_colors
  legend_density <- rep(NA, length(class_labels))
  legend_angle <- rep(NA, length(class_labels))
  if (any(tbl$bin %in% "Special")) {
    legend_labels <- c(legend_labels, "Bin special")
    legend_fill <- c(legend_fill, "#dce6ee")
    legend_density <- c(legend_density, 25)
    legend_angle <- c(legend_angle, 45)
  }
  if (any(tbl$bin %in% "Missing")) {
    legend_labels <- c(legend_labels, "Bin missing")
    legend_fill <- c(legend_fill, "#dce6ee")
    legend_density <- c(legend_density, 25)
    legend_angle <- c(legend_angle, 135)
  }
  usr <- graphics::par("usr")
  leg_x <- mean(usr[1:2])
  leg_y <- usr[3] - if (show_bin_labels) 0.50 * (usr[4] - usr[3]) else 0.30 * (usr[4] - usr[3])
  graphics::legend(
    x = leg_x, y = leg_y,
    legend = legend_labels,
    fill = legend_fill,
    density = legend_density,
    angle = legend_angle,
    border = "black",
    ncol = 2,
    bty = "o",
    bg = "gray95",
    box.col = "gray70",
    xjust = 0.5,
    yjust = 1,
    xpd = NA,
    cex = 0.82,
    pt.cex = 1.2,
    x.intersp = 1.2,
    y.intersp = 1.1
  )
  invisible(x)
}

plot.OptimalBinning <- function(
    x,
    metric = NULL,
    add_special = TRUE,
    add_missing = TRUE,
    style = c("bin", "actual"),
    show_bin_labels = FALSE,
    main = NULL,
    xlab = NULL,
    ylab = NULL,
    type = NULL,
    ...
) {
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)
  graphics::par(bty = "o")
  mar_use <- op$mar
  mar_use[1] <- max(mar_use[1], 8.5)
  mar_use[2] <- max(mar_use[2], 4.8)
  mar_use[4] <- max(mar_use[4], 4.8)
  graphics::par(mar = mar_use)

  if (!isTRUE(x$fitted)) {
    stop("Model is not fitted. Call fit() first.", call. = FALSE)
  }

  target_dtype <- if (!is.null(x$target_dtype)) x$target_dtype else "binary"

  if (!is.null(type)) {
    type <- match.arg(type, c("binning", "woe"))
    metric <- if (identical(type, "binning")) "event_rate" else "woe"
  } else if (is.null(metric)) {
    metric <- if (identical(target_dtype, "continuous")) "mean" else if (identical(target_dtype, "multiclass")) "event_rate" else "woe"
  }

  style <- match.arg(style)
  if (!is.logical(add_special) || length(add_special) != 1L || is.na(add_special)) {
    stop("`add_special` must be TRUE/FALSE.", call. = FALSE)
  }
  if (!is.logical(add_missing) || length(add_missing) != 1L || is.na(add_missing)) {
    stop("`add_missing` must be TRUE/FALSE.", call. = FALSE)
  }
  if (!is.logical(show_bin_labels) || length(show_bin_labels) != 1L || is.na(show_bin_labels)) {
    stop("`show_bin_labels` must be TRUE/FALSE.", call. = FALSE)
  }
  if (show_bin_labels && identical(style, "actual")) {
    stop("`show_bin_labels` is only supported when style = 'bin'.", call. = FALSE)
  }

  bt <- x$bin_table
  if (!is.data.frame(bt) || nrow(bt) == 0L) {
    stop("Binning table is empty.", call. = FALSE)
  }
  if (add_special && !any(bt$bin %in% "Special")) {
    extra <- bt[1, , drop = FALSE]
    extra$bin <- "Special"
    num_cols <- names(extra)[vapply(extra, is.numeric, logical(1))]
    extra[num_cols] <- 0
    bt <- rbind(bt, extra)
  }
  if (add_missing && !any(bt$bin %in% "Missing")) {
    extra <- bt[1, , drop = FALSE]
    extra$bin <- "Missing"
    num_cols <- names(extra)[vapply(extra, is.numeric, logical(1))]
    extra[num_cols] <- 0
    bt <- rbind(bt, extra)
  }

  if (identical(target_dtype, "continuous")) {
    return(invisible(.plot_continuous_optbinning(
      x = x, bt = bt, metric = metric, add_special = add_special,
      add_missing = add_missing, style = style, show_bin_labels = show_bin_labels,
      main = main, xlab = xlab, ylab = ylab, ...
    )))
  }
  if (identical(target_dtype, "multiclass")) {
    return(invisible(.plot_multiclass_optbinning(
      x = x, bt = bt, metric = metric, add_special = add_special,
      add_missing = add_missing, style = style, show_bin_labels = show_bin_labels,
      main = main, xlab = xlab, ylab = ylab, ...
    )))
  }

  metric <- match.arg(metric, c("woe", "event_rate", "iv"))

  metric_col <- switch(metric,
    woe = "woe",
    event_rate = "event_rate",
    iv = "iv"
  )
  metric_label <- switch(metric,
    woe = "WoE",
    event_rate = "Event rate",
    iv = "IV"
  )

  if (!metric_col %in% names(bt)) {
    stop(sprintf("Metric '%s' is unavailable for this model.", metric), call. = FALSE)
  }

  col_event <- "#d62728"
  col_nonevent <- "#1f77b4"
  col_metric <- "black"

  is_special <- bt$bin %in% c("Special")
  is_missing <- bt$bin %in% c("Missing")
  is_core <- !(is_special | is_missing)

  if (identical(style, "actual")) {
    if (!identical(x$dtype, "numerical")) {
      stop("If style = 'actual', dtype must be numerical.", call. = FALSE)
    }
    core <- bt[is_core, , drop = FALSE]
    if (nrow(core) == 0L) {
      stop("No core bins available to plot.", call. = FALSE)
    }
    if (!all(c("event", "non_event") %in% names(core))) {
      stop("`event` and `non_event` columns are required for style = 'actual'.", call. = FALSE)
    }

    breaks <- x$breaks
    if (!is.numeric(breaks) || length(breaks) != (nrow(core) + 1L) || any(!is.finite(breaks))) {
      stop("`style = 'actual'` requires finite numerical breaks.", call. = FALSE)
    }

    metric_vals <- core[[metric_col]]
    if (all(is.na(metric_vals))) {
      stop(sprintf("Metric '%s' has only NA values.", metric), call. = FALSE)
    }

    left <- breaks[-length(breaks)]
    right <- breaks[-1L]
    mid <- (left + right) / 2
    count_total <- core$event + core$non_event

    main_use <- if (is.null(main)) x$name else main
    xlab_use <- if (is.null(xlab)) "x" else xlab
    ylab_use <- if (is.null(ylab)) "Bin count" else ylab

    graphics::plot(
      c(min(left), max(right)),
      c(0, max(count_total, na.rm = TRUE) * 1.1),
      type = "n",
      xlab = xlab_use,
      ylab = ylab_use,
      main = main_use,
      ...
    )

    for (i in seq_len(nrow(core))) {
      graphics::rect(left[i], 0, right[i], core$event[i], col = col_event, border = NA)
      graphics::rect(left[i], core$event[i], right[i], count_total[i], col = col_nonevent, border = NA)
    }

    graphics::par(new = TRUE)
    y_rng <- range(metric_vals, na.rm = TRUE)
    if (!is.finite(y_rng[1]) || !is.finite(y_rng[2])) {
      y_rng <- c(0, 1)
    }
    if (abs(y_rng[2] - y_rng[1]) < .Machine$double.eps) {
      y_rng <- y_rng + c(-0.5, 0.5)
    }
    graphics::plot(c(min(left), max(right)), y_rng, type = "n", axes = FALSE, xlab = "", ylab = "")
    for (i in seq_len(nrow(core))) {
      graphics::lines(c(left[i], right[i]), rep(metric_vals[i], 2L), col = col_metric)
    }
    graphics::points(mid, metric_vals, pch = 16, col = col_metric)
    if (length(breaks) > 2L) {
      graphics::abline(v = breaks[2:(length(breaks) - 1L)], lty = 2, col = "black")
    }
    graphics::axis(4)
    graphics::mtext(metric_label, side = 4, line = 2)
    graphics::box(bty = "o")

    usr <- graphics::par("usr")
    leg_x <- mean(usr[1:2])
    leg_y <- usr[3] - 0.20 * (usr[4] - usr[3])
    graphics::legend(
      x = leg_x, y = leg_y,
      legend = c("Non-event", "Event"),
      fill = c(col_nonevent, col_event),
      ncol = 2,
      bty = "o",
      bg = "gray95",
      box.col = "gray70",
      xjust = 0.5,
      yjust = 1,
      xpd = NA,
      cex = 0.82,
      pt.cex = 1.2,
      x.intersp = 1.2,
      y.intersp = 1.1
    )
    return(invisible(x))
  }

  keep <- is_core | (add_special & is_special) | (add_missing & is_missing)
  tbl <- bt[keep, , drop = FALSE]
  if (nrow(tbl) == 0L) {
    stop("No bins available to plot with current filters.", call. = FALSE)
  }
  if (!all(c("event", "non_event") %in% names(tbl))) {
    stop("`event` and `non_event` columns are required for plotting.", call. = FALSE)
  }

  metric_vals <- tbl[[metric_col]]
  if (all(is.na(metric_vals))) {
    stop(sprintf("Metric '%s' has only NA values.", metric), call. = FALSE)
  }

  ids <- seq_len(nrow(tbl)) - 1L
  labels <- if (show_bin_labels) as.character(tbl$bin) else as.character(ids)

  main_use <- if (is.null(main)) x$name else main
  xlab_use <- if (is.null(xlab)) {
    if (show_bin_labels) "Bin" else "Bin ID"
  } else {
    xlab
  }
  ylab_use <- if (is.null(ylab)) "Bin count" else ylab
  y_top <- max(tbl$event + tbl$non_event, na.rm = TRUE)
  y_lim <- c(0, y_top * 1.1)
  centers <- graphics::barplot(
    rbind(tbl$event, tbl$non_event),
    col = c(col_event, col_nonevent),
    border = NA,
    names.arg = labels,
    xlab = xlab_use,
    ylab = ylab_use,
    ylim = y_lim,
    main = main_use,
    ...
  )
  x_lim <- range(centers) + c(-0.6, 0.6)
  bar_half_width <- if (length(centers) > 1L) min(diff(centers)) * 0.4 else 0.4
  special_idx <- which(tbl$bin %in% "Special")
  missing_idx <- which(tbl$bin %in% "Missing")
  if (length(special_idx) > 0L) {
    for (i in special_idx) {
      y_top_i <- tbl$event[i] + tbl$non_event[i]
      graphics::rect(
        centers[i] - bar_half_width, 0,
        centers[i] + bar_half_width, y_top_i,
        density = 25, angle = 45, col = "black", border = NA
      )
    }
  }
  if (length(missing_idx) > 0L) {
    for (i in missing_idx) {
      y_top_i <- tbl$event[i] + tbl$non_event[i]
      graphics::rect(
        centers[i] - bar_half_width, 0,
        centers[i] + bar_half_width, y_top_i,
        density = 25, angle = 135, col = "black", border = NA
      )
    }
  }

  core_idx <- which(!(tbl$bin %in% c("Special", "Missing")))
  graphics::par(new = TRUE)
  y_rng <- range(metric_vals, na.rm = TRUE)
  if (!is.finite(y_rng[1]) || !is.finite(y_rng[2])) {
    y_rng <- c(0, 1)
  }
  if (abs(y_rng[2] - y_rng[1]) < .Machine$double.eps) {
    y_rng <- y_rng + c(-0.5, 0.5)
  }
  graphics::plot(
    centers, metric_vals,
    type = "n", axes = FALSE, xlab = "", ylab = "",
    xlim = x_lim, ylim = y_rng
  )
  if (length(core_idx) > 0L) {
    graphics::lines(centers[core_idx], metric_vals[core_idx], type = "b", pch = 16, col = col_metric)
  }
  side_idx <- setdiff(seq_len(nrow(tbl)), core_idx)
  if (length(side_idx) > 0L) {
    graphics::points(centers[side_idx], metric_vals[side_idx], pch = 16, col = col_metric)
  }
  graphics::axis(4)
  graphics::mtext(metric_label, side = 4, line = 2)
  graphics::box(bty = "o")

  legend_labels <- c("Non-event", "Event")
  legend_fill <- c(col_nonevent, col_event)
  legend_density <- c(NA, NA)
  legend_angle <- c(NA, NA)
  if (length(special_idx) > 0L) {
    legend_labels <- c(legend_labels, "Bin special")
    legend_fill <- c(legend_fill, "gray80")
    legend_density <- c(legend_density, NA)
    legend_angle <- c(legend_angle, NA)
  }
  if (length(missing_idx) > 0L) {
    legend_labels <- c(legend_labels, "Bin missing")
    legend_fill <- c(legend_fill, "gray60")
    legend_density <- c(legend_density, NA)
    legend_angle <- c(legend_angle, NA)
  }

  usr <- graphics::par("usr")
  leg_x <- mean(usr[1:2])
  leg_y <- usr[3] - 0.20 * (usr[4] - usr[3])
  graphics::legend(
    x = leg_x, y = leg_y,
    legend = legend_labels,
    fill = legend_fill,
    density = legend_density,
    angle = legend_angle,
    border = "black",
    ncol = 2,
    bty = "o",
    bg = "gray95",
    box.col = "gray70",
    xjust = 0.5,
    yjust = 1,
    xpd = NA,
    cex = 0.82,
    pt.cex = 1.2,
    x.intersp = 1.2,
    y.intersp = 1.1
  )

  invisible(x)
}

#' @export
plot.OptimalBinning2D <- function(x, type = c("binning", "woe"), main = NULL, xlab = NULL, ylab = NULL, ...) {
  if (!isTRUE(x$fitted)) {
    stop("Model is not fitted. Call fit() first.", call. = FALSE)
  }
  type <- match.arg(type)
  bt <- x$bin_table
  z <- matrix(NA_real_, nrow = length(x$breaks_x) - 1L, ncol = length(x$breaks_y) - 1L)
  for (i in seq_len(nrow(bt))) {
    val <- if (identical(type, "woe") && "woe" %in% names(bt) && !all(is.na(bt$woe))) bt$woe[i] else bt$event_rate[i]
    z[bt$ix[i], bt$iy[i]] <- val
  }
  main_use <- if (is.null(main)) sprintf("Optimal Binning 2D: %s vs %s", x$name_x, x$name_y) else main
  xlab_use <- if (is.null(xlab)) x$name_x else xlab
  ylab_use <- if (is.null(ylab)) x$name_y else ylab
  graphics::image(
    x = seq_len(nrow(z)),
    y = seq_len(ncol(z)),
    z = z,
    col = grDevices::hcl.colors(30, "YlOrRd", rev = FALSE),
    xlab = xlab_use,
    ylab = ylab_use,
    main = main_use,
    ...
  )
  invisible(x)
}
