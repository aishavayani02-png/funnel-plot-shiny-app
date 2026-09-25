# Plot generation functions. These functions work from fitted models, prepared
# effect data, and simple option lists rather than Shiny reactives.

draw_regression_line <- function(res, predictor, yaxis, extrapolate,
                                 col, lty, lwd = 2) {
  se_obs <- sqrt(res$vi)
  se_obs <- se_obs[is.finite(se_obs) & se_obs > 0]
  if (length(se_obs) < 2) return(FALSE)

  se_start <- if (isTRUE(extrapolate)) 0 else min(se_obs)
  if (yaxis %in% c("seinv", "vinv") && se_start == 0) {
    se_start <- max(min(se_obs) * 0.05, .Machine$double.eps)
  }

  se_seq <- seq(se_start, max(se_obs), length.out = 200)
  test <- tryCatch(
    metafor::regtest(res, model = "rma", predictor = predictor),
    error = function(e) NULL
  )
  if (is.null(test) || is.null(test$fit)) return(FALSE)

  b <- as.numeric(stats::coef(test$fit))
  if (length(b) < 2 || any(!is.finite(b[1:2]))) return(FALSE)

  x <- if (identical(predictor, "vi")) b[1] + b[2] * se_seq^2 else b[1] + b[2] * se_seq
  y <- y_from_se(se_seq, yaxis)
  keep <- is.finite(x) & is.finite(y)
  if (sum(keep) < 2) return(FALSE)

  graphics::lines(x[keep], y[keep], lwd = lwd, lty = lty, col = col)
  TRUE
}

draw_funnel_plot <- function(res, effect_data, plot_options,
                             trimfill_model = NULL, refline = NA_real_,
                             draw_legend = TRUE) {
  natural <- plot_options$natural_scale
  obj_to_plot <- if (!is.null(trimfill_model)) trimfill_model else res
  has_trimfill <- !is.null(trimfill_model)

  old_mar <- graphics::par("mar")
  old_xpd <- graphics::par("xpd")
  old_mgp <- graphics::par("mgp")
  on.exit(graphics::par(mar = old_mar, xpd = old_xpd, mgp = old_mgp), add = TRUE)

  plot_margin <- if (isTRUE(draw_legend)) c(13.8, 4.4, 4.1, 2.1) else c(5.4, 4.4, 4.1, 2.1)
  graphics::par(mar = plot_margin, mgp = c(2.2, 0.7, 0))

  xlab_txt <- effect_label(plot_options$datatype, plot_options$measure, natural)
  atransf_fun <- if (natural) exp else NULL

  main_title <- paste0(
    if (identical(plot_options$region_shading, "contour")) "Contour-Enhanced Funnel Plot" else "Funnel Plot",
    " - ", model_name(plot_options$method)
  )

  funnel_args <- list(
    x = obj_to_plot,
    yaxis = plot_options$yaxis,
    xlab = xlab_txt,
    main = main_title,
    legend = FALSE,
    steps = plot_steps,
    digits = c(plot_digits, plot_digits),
    lty = c(3, 0),
    colci = "gray45",
    back = "white"
  )

  if (!is.null(atransf_fun)) funnel_args$atransf <- atransf_fun
  if (!has_trimfill) {
    funnel_args$pch <- 19
    funnel_args$col <- "black"
  }

  if (identical(plot_options$region_shading, "contour")) {
    funnel_args$refline <- 0
    funnel_args$level <- c(90, 95, 99)
    funnel_args$shade <- c("white", "gray85", "gray70")
    funnel_args$back <- "gray55"
  } else {
    funnel_args$level <- plot_level
    funnel_args$refline <- if (is.finite(refline)) refline else as.numeric(stats::coef(res))[1]
    funnel_args$shade <- if (
      identical(plot_options$region_shading, "heterogeneity") &&
        identical(plot_options$method, "REML")
    ) "gray85" else "white"
    if (identical(plot_options$region_shading, "heterogeneity") &&
        identical(plot_options$method, "REML")) {
      funnel_args$addtau2 <- TRUE
    }
  }

  do.call(metafor::funnel, funnel_args)

  legend_labels <- character(0)
  legend_lty <- numeric(0)
  legend_lwd <- numeric(0)
  legend_col <- character(0)
  legend_pch <- numeric(0)
  legend_ptbg <- character(0)

  add_line_legend <- function(label, lty, lwd, col) {
    legend_labels <<- c(legend_labels, label)
    legend_lty <<- c(legend_lty, lty)
    legend_lwd <<- c(legend_lwd, lwd)
    legend_col <<- c(legend_col, col)
    legend_pch <<- c(legend_pch, NA)
    legend_ptbg <<- c(legend_ptbg, NA)
  }

  add_point_legend <- function(label, pch, ptbg, col = "black") {
    legend_labels <<- c(legend_labels, label)
    legend_lty <<- c(legend_lty, NA)
    legend_lwd <<- c(legend_lwd, NA)
    legend_col <<- c(legend_col, col)
    legend_pch <<- c(legend_pch, pch)
    legend_ptbg <<- c(legend_ptbg, ptbg)
  }

  if (identical(plot_options$region_shading, "contour")) {
    add_point_legend("p > 0.10", 22, "white")
    add_point_legend("0.05 < p <= 0.10", 22, "gray85")
    add_point_legend("0.01 < p <= 0.05", 22, "gray70")
    add_point_legend("p <= 0.01", 22, "gray55")
    add_line_legend("P-value contour boundaries", 3, 1, "gray45")
    graphics::abline(v = 0, lwd = 1.5, lty = 2, col = "gray35")
    add_line_legend("Null effect line", 2, 1.5, "gray35")
  } else if (identical(plot_options$region_shading, "heterogeneity") &&
             identical(plot_options$method, "REML")) {
    add_point_legend("Heterogeneity band", 22, "gray85")
    add_line_legend("95% limits including tau-squared", 3, 1, "gray45")
  } else {
    add_line_legend("95% pseudo confidence limits", 3, 1, "gray45")
  }

  add_point_legend("Observed studies", 19, "black")

  if (is.finite(refline)) {
    graphics::abline(v = refline, lwd = 2, lty = 1, col = "#1b6ca8")
    add_line_legend(
      if (identical(plot_options$refline_mode, "custom")) "User reference line" else "Current pooled estimate",
      1, 2, "#1b6ca8"
    )
  }

  if (has_trimfill) {
    graphics::abline(v = as.numeric(trimfill_model$b)[1], lwd = 2, lty = 4, col = "#8e44ad")
    if (isTRUE(trimfill_model$k0 > 0)) {
      add_point_legend("Imputed studies", 21, "white")
    }
    add_line_legend("Trim-and-fill adjusted estimate", 4, 2, "#8e44ad")
  }

  if (identical(plot_options$biastest, "egger") && isTRUE(plot_options$show_test_line)) {
    if (draw_regression_line(res, "sei", plot_options$yaxis, TRUE, "#4c78a8", 3)) {
      add_line_legend("Egger regression line", 3, 2, "#4c78a8")
    }
  }

  if (identical(plot_options$show_limit, "yes")) {
    predictors <- plot_options$limit_predictors
    if ("sei" %in% predictors) {
      if (draw_regression_line(res, "sei", plot_options$yaxis,
                               plot_options$limit_extrapolate, "#1b9e77", 1)) {
        add_line_legend("Limit line: standard error", 1, 2, "#1b9e77")
      }
    }
    if ("vi" %in% predictors) {
      if (draw_regression_line(res, "vi", plot_options$yaxis,
                               plot_options$limit_extrapolate, "#d95f02", 2)) {
        add_line_legend("Limit line: sampling variance", 2, 2, "#d95f02")
      }
    }
  }

  if (identical(plot_options$label_mode, "key")) {
    xs <- res$yi
    ys <- yvals_from_vi(res$vi, plot_options$yaxis)
    graphics::text(xs, ys, labels = seq_along(xs), cex = 0.65, pos = 4, offset = 0.25, xpd = NA)
  } else if (identical(plot_options$label_mode, "all")) {
    xs <- res$yi
    ys <- yvals_from_vi(res$vi, plot_options$yaxis)
    graphics::text(xs, ys, labels = effect_data$labels, cex = 0.55, pos = 4, offset = 0.25, xpd = FALSE)
  }

  legend_info <- list(
    labels = legend_labels,
    pch = legend_pch,
    pt.bg = legend_ptbg,
    lty = legend_lty,
    lwd = legend_lwd,
    col = legend_col,
    ncol = if (length(legend_labels) > 9) 3 else 2,
    cex = if (length(legend_labels) > 9) 0.78 else 0.9
  )

  if (isTRUE(draw_legend) && length(legend_labels) > 0) {
    graphics::legend("bottom",
                     legend = legend_labels,
                     pch = legend_pch,
                     pt.bg = legend_ptbg,
                     lty = legend_lty,
                     lwd = legend_lwd,
                     col = legend_col,
                     cex = legend_info$cex,
                     pt.cex = 1.05,
                     bty = "n",
                     xpd = TRUE,
                     inset = c(0, -0.38),
                     ncol = legend_info$ncol)
  }

  invisible(legend_info)
}

draw_results_footer <- function(result_lines) {
  lines <- unlist(lapply(result_lines, strwrap, width = 115))
  lines <- lines[seq_len(min(length(lines), 10))]

  graphics::par(mar = c(0.2, 0.2, 0.2, 0.2))
  graphics::plot.new()
  graphics::text(0.02, 0.96, "Analysis results", adj = c(0, 1), cex = 0.95, font = 2)

  y <- 0.82
  for (line in lines) {
    graphics::text(0.02, y, line, adj = c(0, 1), cex = 0.82)
    y <- y - 0.12
    if (y < 0.08) break
  }
}

draw_plot_legend_footer <- function(legend_info) {
  if (is.null(legend_info) || length(legend_info$labels) == 0) return()

  graphics::par(mar = c(0.1, 0.2, 0.1, 0.2))
  graphics::plot.new()
  graphics::legend("center",
                   legend = legend_info$labels,
                   pch = legend_info$pch,
                   pt.bg = legend_info$pt.bg,
                   lty = legend_info$lty,
                   lwd = legend_info$lwd,
                   col = legend_info$col,
                   cex = min(1.05, legend_info$cex + 0.12),
                   pt.cex = 1.1,
                   bty = "n",
                   ncol = legend_info$ncol)
}

draw_study_key_footer <- function(key_lines) {
  n <- length(key_lines)
  split_at <- ceiling(n / 2)
  left <- key_lines[seq_len(split_at)]
  right <- if (split_at < n) key_lines[(split_at + 1):n] else character(0)

  graphics::par(mar = c(0.2, 0.2, 0.2, 0.2))
  graphics::plot.new()
  graphics::text(0.02, 0.96, "Study key", adj = c(0, 1), cex = 1, font = 2)

  draw_column <- function(x, values) {
    if (length(values) == 0) return()
    yvals <- seq(0.78, 0.12, length.out = length(values))
    for (i in seq_along(values)) {
      graphics::text(x, yvals[i], values[i], adj = c(0, 1), cex = 0.8)
    }
  }

  draw_column(0.02, left)
  draw_column(0.52, right)
}
