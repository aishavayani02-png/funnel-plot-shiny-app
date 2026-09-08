# =============================================================================
# Shiny App: Interactive Funnel Plot with bias tests and trim-and-fill
# =============================================================================

library(shiny)
library(metafor)
library(metadat)

# -----------------------------------------------------------------------------
# Small display helpers
# -----------------------------------------------------------------------------
is_log_effect <- function(datatype, measure) {
  identical(datatype, "binary") && measure %in% c("OR", "RR")
}

fmt_num <- function(x, digits = 2) {
  x <- as.numeric(x)[1]
  if (!is.finite(x)) return("NA")
  formatC(x, format = "f", digits = digits)
}

fmt_p <- function(x) {
  out <- format.pval(as.numeric(x)[1], digits = 3, eps = 0.001)
  gsub("<", "&lt;", out, fixed = TRUE)
}

fmt_p_phrase <- function(x, html = TRUE) {
  p <- if (html) fmt_p(x) else gsub("&lt;", "<", fmt_p(x), fixed = TRUE)
  if (startsWith(p, if (html) "&lt;" else "<")) paste0("p ", p) else paste0("p = ", p)
}

effect_label <- function(datatype, measure, natural_scale = TRUE) {
  switch(measure,
         "OR" = if (natural_scale) "Odds Ratio" else "Log Odds Ratio",
         "RR" = if (natural_scale) "Risk Ratio" else "Log Risk Ratio",
         "RD" = "Risk Difference",
         "MD" = "Mean Difference",
         "SMD" = "Standardized Mean Difference",
         "Effect")
}

analysis_to_display <- function(x, datatype, measure, natural_scale = TRUE) {
  if (is_log_effect(datatype, measure) && natural_scale) exp(x) else x
}

display_to_analysis <- function(x, datatype, measure, natural_scale = TRUE) {
  if (is_log_effect(datatype, measure) && natural_scale) log(x) else x
}

yvals_from_vi <- function(vi, yaxis) {
  se <- sqrt(vi)
  switch(yaxis,
         "sei" = se,
         "seinv" = 1 / se,
         "vi" = vi,
         "vinv" = 1 / vi)
}

y_from_se <- function(se, yaxis) {
  switch(yaxis,
         "sei" = se,
         "seinv" = 1 / se,
         "vi" = se^2,
         "vinv" = 1 / (se^2))
}

model_name <- function(method) {
  if (identical(method, "REML")) "Random-effects (REML)" else "Fixed-effects"
}

link <- function(url, label) {
  paste0("<a href='", url, "' target='_blank' rel='noopener noreferrer'>", label, "</a>")
}

plot_digits <- 2L
plot_steps <- 5L
plot_level <- 95

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .well { max-width: 360px; }
      .result-block {
        border-top: 1px solid #ddd;
        padding-top: 12px;
        margin-top: 12px;
      }
      .method-links {
        color: #444;
        font-size: 0.95em;
      }
      .small-note {
        color: #555;
        font-size: 0.95em;
      }
    "))
  ),
  titlePanel("Funnel Plot with Publication Bias Tests"),

  sidebarLayout(
    sidebarPanel(
      radioButtons("datatype", "Type of Data:",
                   choices = c("Binary Outcomes" = "binary",
                               "Continuous Outcomes" = "continuous"),
                   selected = "binary"),

      uiOutput("measure_ui"),

      radioButtons("method", "Meta-Analysis Model:",
                   choices = c("Fixed-effects" = "FE",
                               "Random-effects (REML)" = "REML"),
                   selected = "FE"),

      radioButtons("reflineMode", "Reference Line:",
                   choices = c("Current pooled estimate" = "pooled",
                               "User-defined value" = "custom",
                               "No reference line" = "none"),
                   selected = "pooled"),
      uiOutput("custom_refline_ui"),

      radioButtons("yaxis", "Y-Axis Scale:",
                   choices = c("Standard Error" = "sei",
                               "Inverse Standard Error" = "seinv",
                               "Variance" = "vi",
                               "Inverse Variance" = "vinv"),
                   selected = "sei"),

      uiOutput("xscale_ui"),

      radioButtons("regionShading", "Region Shading:",
                   choices = c("None" = "none",
                               "Contour-enhanced p-value regions" = "contour",
                               "Heterogeneity band" = "heterogeneity"),
                   selected = "none"),

      uiOutput("biastest_ui"),
      conditionalPanel(
        condition = "input.biastest == 'egger'",
        checkboxInput("showTestLine", "Show Egger regression line", value = FALSE)
      ),

      radioButtons("showLimit", "Limit Estimate Lines:",
                   choices = c("No" = "no", "Yes" = "yes"),
                   selected = "no"),
      conditionalPanel(
        condition = "input.showLimit == 'yes'",
        checkboxGroupInput("limitPredictors", "Limit Line Predictors:",
                           choices = c("Standard error" = "sei",
                                       "Sampling variance" = "vi"),
                           selected = "sei"),
        checkboxInput("limitExtrapolate", "Extrapolate to SE = 0", value = TRUE)
      ),

      radioButtons("trimfill", "Trim-and-Fill Adjustment:",
                   choices = c("No" = "no", "Yes" = "yes"),
                   selected = "no"),

      radioButtons("labelMode", "Study Labels:",
                   choices = c("None" = "none",
                               "Label all studies inside plot" = "all",
                               "Number studies and show key" = "key"),
                   selected = "none"),

      sliderInput("plotHeight", "Plot Height:",
                  min = 420, max = 820, value = 560, step = 20)
    ),

    mainPanel(
      uiOutput("tabs_ui")
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  output$measure_ui <- renderUI({
    if (identical(input$datatype, "binary")) {
      radioButtons("measure", "Effect Size:",
                   choices = c("Odds Ratio" = "OR",
                               "Risk Ratio" = "RR",
                               "Risk Difference" = "RD"),
                   selected = "OR")
    } else {
      radioButtons("measure", "Effect Size:",
                   choices = c("Mean Difference" = "MD",
                               "Standardized Mean Difference" = "SMD"),
                   selected = "MD")
    }
  })

  output$xscale_ui <- renderUI({
    req(input$datatype, input$measure)
    if (is_log_effect(input$datatype, input$measure)) {
      checkboxInput("naturalScale",
                    paste0("Show x-axis on natural ", input$measure, " scale"),
                    value = TRUE)
    }
  })

  output$biastest_ui <- renderUI({
    req(input$datatype)

    choices <- c("None" = "none", "Egger's test" = "egger")
    if (identical(input$datatype, "binary")) {
      choices <- c(choices, "Peters' test" = "peters")
    }

    current <- isolate(input$biastest)
    selected <- if (!is.null(current) && current %in% unname(choices)) current else "none"

    radioButtons("biastest", "Publication Bias Test:",
                 choices = choices,
                 selected = selected)
  })

  output$custom_refline_ui <- renderUI({
    req(input$datatype, input$measure)
    if (!identical(input$reflineMode, "custom")) return(NULL)

    natural <- isTRUE(input$naturalScale)
    label <- if (is_log_effect(input$datatype, input$measure) && natural) {
      paste0("Custom reference line (", input$measure, " scale):")
    } else {
      "Custom reference line (analysis scale):"
    }
    default_value <- if (is_log_effect(input$datatype, input$measure) && natural) 1 else 0

    numericInput("customRefline", label, value = default_value, step = 0.1)
  })

  effect_data <- reactive({
    req(input$datatype, input$measure)

    if (identical(input$datatype, "binary")) {
      dat <- metadat::dat.bcg
      esc <- escalc(measure = input$measure,
                    ai = dat$tpos, bi = dat$tneg,
                    ci = dat$cpos, di = dat$cneg,
                    data = dat)
      labels <- if (all(c("author", "year") %in% names(dat))) {
        paste0(dat$author, " (", dat$year, ")")
      } else {
        paste("Study", dat$trial)
      }
    } else {
      dat <- metadat::dat.normand1999
      esc <- escalc(measure = input$measure,
                    m1i = dat$m1i, sd1i = dat$sd1i, n1i = dat$n1i,
                    m2i = dat$m2i, sd2i = dat$sd2i, n2i = dat$n2i,
                    data = dat)
      labels <- if ("study" %in% names(dat)) paste("Study", dat$study) else rownames(dat)
    }

    list(dat = dat, esc = esc, labels = labels)
  })

  model <- reactive({
    dat <- effect_data()
    tryCatch(
      rma(yi, vi, data = dat$esc, method = input$method),
      error = function(e) NULL
    )
  })

  x_uses_natural_scale <- reactive({
    is_log_effect(input$datatype, input$measure) && isTRUE(input$naturalScale)
  })

  selected_refline <- reactive({
    res <- model()
    req(res)

    if (identical(input$reflineMode, "none")) return(NA_real_)
    if (identical(input$reflineMode, "pooled")) return(as.numeric(coef(res))[1])

    custom_value <- suppressWarnings(as.numeric(input$customRefline))
    if (!is.finite(custom_value)) return(NA_real_)
    if (is_log_effect(input$datatype, input$measure) &&
        x_uses_natural_scale() &&
        custom_value <= 0) {
      return(NA_real_)
    }

    display_to_analysis(custom_value, input$datatype, input$measure, x_uses_natural_scale())
  })

  trimfill_model <- reactive({
    req(input$trimfill)
    if (!identical(input$trimfill, "yes")) return(NULL)

    res <- model()
    req(res)

    tryCatch(trimfill(res), error = function(e) NULL)
  })

  bias_test <- reactive({
    req(input$biastest)
    if (identical(input$biastest, "none")) return(NULL)
    if (identical(input$biastest, "peters") && !identical(input$datatype, "binary")) return(NULL)

    res <- model()
    req(res)

    tryCatch({
      if (identical(input$biastest, "egger")) {
        regtest(res, model = "rma", predictor = "sei")
      } else {
        regtest(res, model = "rma", predictor = "ninv")
      }
    }, error = function(e) NULL)
  })

  pvalue_distribution <- reactive({
    res <- model()
    req(res)

    p_vals <- 2 * pnorm(-abs(res$yi / sqrt(res$vi)))
    list(
      total = length(p_vals),
      veryhigh = sum(p_vals <= 0.01),
      high = sum(p_vals > 0.01 & p_vals <= 0.05),
      border = sum(p_vals > 0.05 & p_vals <= 0.10),
      nonsig = sum(p_vals > 0.10)
    )
  })

  draw_regression_line <- function(res, predictor, yaxis, extrapolate, col, lty, lwd = 2) {
    se_obs <- sqrt(res$vi)
    se_obs <- se_obs[is.finite(se_obs) & se_obs > 0]
    if (length(se_obs) < 2) return(FALSE)

    se_start <- if (isTRUE(extrapolate)) 0 else min(se_obs)
    if (yaxis %in% c("seinv", "vinv") && se_start == 0) {
      se_start <- max(min(se_obs) * 0.05, .Machine$double.eps)
    }

    se_seq <- seq(se_start, max(se_obs), length.out = 200)
    test <- tryCatch(regtest(res, model = "rma", predictor = predictor), error = function(e) NULL)
    if (is.null(test) || is.null(test$fit)) return(FALSE)

    b <- as.numeric(coef(test$fit))
    if (length(b) < 2 || any(!is.finite(b[1:2]))) return(FALSE)

    x <- if (identical(predictor, "vi")) b[1] + b[2] * se_seq^2 else b[1] + b[2] * se_seq
    y <- y_from_se(se_seq, yaxis)
    keep <- is.finite(x) & is.finite(y)
    if (sum(keep) < 2) return(FALSE)

    lines(x[keep], y[keep], lwd = lwd, lty = lty, col = col)
    TRUE
  }

  predict_limit_value <- function(fit, target) {
    pred <- tryCatch(predict(fit, newmods = target), error = function(e) NULL)
    if (!is.null(pred) && all(c("pred", "ci.lb", "ci.ub") %in% names(pred))) {
      return(as.numeric(c(pred$pred, pred$ci.lb, pred$ci.ub)))
    }

    b <- as.numeric(coef(fit))
    V <- tryCatch(vcov(fit), error = function(e) NULL)
    if (length(b) < 2 || is.null(V) || nrow(V) < 2 || ncol(V) < 2) return(NULL)

    x <- c(1, target)
    est <- sum(x * b[1:2])
    se <- sqrt(drop(t(x) %*% V[1:2, 1:2] %*% x))
    if (!is.finite(est) || !is.finite(se)) return(NULL)

    crit <- qnorm(0.975)
    c(est, est - crit * se, est + crit * se)
  }

  limit_line_estimates <- function(res) {
    if (!identical(input$showLimit, "yes")) return(list())

    predictors <- intersect(c("sei", "vi"), input$limitPredictors)
    if (length(predictors) == 0) return(list())

    se_obs <- sqrt(res$vi)
    se_obs <- se_obs[is.finite(se_obs) & se_obs > 0]
    if (length(se_obs) < 2) return(list())

    min_se <- min(se_obs)
    natural <- x_uses_natural_scale()
    label <- effect_label(input$datatype, input$measure, natural)

    rows <- lapply(predictors, function(predictor) {
      test <- tryCatch(regtest(res, model = "rma", predictor = predictor), error = function(e) NULL)
      if (is.null(test) || is.null(test$fit)) return(NULL)

      if (identical(predictor, "vi")) {
        target <- if (isTRUE(input$limitExtrapolate)) 0 else min_se^2
        location <- if (isTRUE(input$limitExtrapolate)) {
          "extrapolated to sampling variance = 0"
        } else {
          paste0("at the most precise observed study (sampling variance = ",
                 fmt_num(min_se^2, plot_digits), ")")
        }
        predictor_label <- "Sampling variance"
      } else {
        target <- if (isTRUE(input$limitExtrapolate)) 0 else min_se
        location <- if (isTRUE(input$limitExtrapolate)) {
          "extrapolated to SE = 0"
        } else {
          paste0("at the most precise observed study (SE = ",
                 fmt_num(min_se, plot_digits), ")")
        }
        predictor_label <- "Standard error"
      }

      vals <- predict_limit_value(test$fit, target)
      if (is.null(vals) || any(!is.finite(vals))) return(NULL)
      vals <- analysis_to_display(vals, input$datatype, input$measure, natural)

      list(
        predictor = predictor,
        predictor_label = predictor_label,
        location = location,
        effect_label = label,
        estimate = vals[1],
        ci.lb = vals[2],
        ci.ub = vals[3]
      )
    })

    Filter(Negate(is.null), rows)
  }

  draw_funnel_plot <- function(res, draw_legend = TRUE) {
    natural <- x_uses_natural_scale()
    tf <- trimfill_model()
    obj_to_plot <- if (!is.null(tf)) tf else res
    has_trimfill <- !is.null(tf)
    refline <- selected_refline()
    dat <- effect_data()
    old_mar <- par("mar")
    old_xpd <- par("xpd")
    old_mgp <- par("mgp")
    on.exit(par(mar = old_mar, xpd = old_xpd, mgp = old_mgp), add = TRUE)
    plot_margin <- if (isTRUE(draw_legend)) c(13.8, 4.4, 4.1, 2.1) else c(5.4, 4.4, 4.1, 2.1)
    par(mar = plot_margin, mgp = c(2.2, 0.7, 0))

    xlab_txt <- effect_label(input$datatype, input$measure, natural)
    atransf_fun <- if (natural) exp else NULL

    main_title <- paste0(
      if (identical(input$regionShading, "contour")) "Contour-Enhanced Funnel Plot" else "Funnel Plot",
      " - ", model_name(input$method)
    )

    funnel_args <- list(
      x = obj_to_plot,
      yaxis = input$yaxis,
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

    if (identical(input$regionShading, "contour")) {
      # Contours indicate study-level p-value regions around the null effect.
      funnel_args$refline <- 0
      funnel_args$level <- c(90, 95, 99)
      funnel_args$shade <- c("white", "gray85", "gray70")
      funnel_args$back <- "gray55"
    } else {
      funnel_args$level <- plot_level
      funnel_args$refline <- if (is.finite(refline)) refline else as.numeric(coef(res))[1]
      funnel_args$shade <- if (
        identical(input$regionShading, "heterogeneity") &&
          identical(input$method, "REML")
      ) "gray85" else "white"
      if (identical(input$regionShading, "heterogeneity") && identical(input$method, "REML")) {
        funnel_args$addtau2 <- TRUE
      }
    }

    do.call(funnel, funnel_args)

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

    if (identical(input$regionShading, "contour")) {
      add_point_legend("p > 0.10", 22, "white")
      add_point_legend("0.05 < p <= 0.10", 22, "gray85")
      add_point_legend("0.01 < p <= 0.05", 22, "gray70")
      add_point_legend("p <= 0.01", 22, "gray55")
      add_line_legend("P-value contour boundaries", 3, 1, "gray45")
      abline(v = 0, lwd = 1.5, lty = 2, col = "gray35")
      add_line_legend("Null effect line", 2, 1.5, "gray35")
    } else if (identical(input$regionShading, "heterogeneity") && identical(input$method, "REML")) {
      add_point_legend("Heterogeneity band", 22, "gray85")
      add_line_legend("95% limits including tau-squared", 3, 1, "gray45")
    } else {
      add_line_legend("95% pseudo confidence limits", 3, 1, "gray45")
    }

    add_point_legend("Observed studies", 19, "black")

    if (is.finite(refline)) {
      abline(v = refline, lwd = 2, lty = 1, col = "#1b6ca8")
      add_line_legend(
        if (identical(input$reflineMode, "custom")) "User reference line" else "Current pooled estimate",
        1, 2, "#1b6ca8"
      )
    }

    if (has_trimfill) {
      abline(v = as.numeric(tf$b)[1], lwd = 2, lty = 4, col = "#8e44ad")
      if (isTRUE(tf$k0 > 0)) {
        add_point_legend("Imputed studies", 21, "white")
      }
      add_line_legend("Trim-and-fill adjusted estimate", 4, 2, "#8e44ad")
    }

    if (identical(input$biastest, "egger") && isTRUE(input$showTestLine)) {
      if (draw_regression_line(res, "sei", input$yaxis, TRUE, "#4c78a8", 3)) {
        add_line_legend("Egger regression line", 3, 2, "#4c78a8")
      }
    }

    if (identical(input$showLimit, "yes")) {
      predictors <- input$limitPredictors
      if ("sei" %in% predictors) {
        if (draw_regression_line(res, "sei", input$yaxis, input$limitExtrapolate, "#1b9e77", 1)) {
          add_line_legend("Limit line: standard error", 1, 2, "#1b9e77")
        }
      }
      if ("vi" %in% predictors) {
        if (draw_regression_line(res, "vi", input$yaxis, input$limitExtrapolate, "#d95f02", 2)) {
          add_line_legend("Limit line: sampling variance", 2, 2, "#d95f02")
        }
      }
    }

    if (identical(input$labelMode, "key")) {
      xs <- res$yi
      ys <- yvals_from_vi(res$vi, input$yaxis)
      text(xs, ys, labels = seq_along(xs), cex = 0.65, pos = 4, offset = 0.25, xpd = NA)
    } else if (identical(input$labelMode, "all")) {
      xs <- res$yi
      ys <- yvals_from_vi(res$vi, input$yaxis)
      text(xs, ys, labels = dat$labels, cex = 0.55, pos = 4, offset = 0.25, xpd = FALSE)
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
      legend("bottom",
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

  output$funnelMain <- renderPlot({
    res <- model()
    validate(need(!is.null(res), "Waiting for a valid model..."))
    draw_funnel_plot(res)
  }, height = function() input$plotHeight)

  output$analysisSummary <- renderUI({
    res <- model()
    validate(need(!is.null(res), "Waiting for a valid model..."))

    natural <- x_uses_natural_scale()
    vals <- analysis_to_display(c(res$b, res$ci.lb, res$ci.ub),
                                input$datatype, input$measure, natural)
    label <- effect_label(input$datatype, input$measure, natural)
    tau_text <- if (identical(input$method, "REML")) {
      paste0(" The estimated heterogeneity variance tau-squared is <b>",
             fmt_num(res$tau2, 3), "</b>.")
    } else {
      ""
    }

    ref_text <- switch(input$reflineMode,
                       "pooled" = "The vertical blue line shows the current pooled estimate.",
                       "custom" = "The vertical blue line shows the user-defined reference value.",
                       "none" = "No vertical reference line is currently shown.")

    HTML(paste0(
      "<div class='result-block'>",
      "<p><b>Current Analysis</b></p>",
      "<p>", model_name(input$method), " pooled <b>", label, "</b>: <b>",
      fmt_num(vals[1], plot_digits), "</b> with 95% CI <b>[",
      fmt_num(vals[2], plot_digits), ", ",
      fmt_num(vals[3], plot_digits), "]</b>.", tau_text, "</p>",
      "<p>", ref_text, " The funnel confidence region is fixed at 95% for the standard plot.</p>",
      "</div>"
    ))
  })

  output$biasTestOutput <- renderUI({
    if (identical(input$biastest, "none")) return(NULL)
    if (identical(input$biastest, "peters") && !identical(input$datatype, "binary")) return(NULL)

    res <- model()
    validate(need(!is.null(res), "Waiting for a valid model..."))
    test <- bias_test()
    if (is.null(test)) {
      return(HTML("<div class='result-block'><p style='color:red'><b>Publication Bias Test</b></p><p>The selected test could not be performed for this model.</p></div>"))
    }

    test_name <- if (identical(input$biastest, "egger")) {
      "Egger's regression test"
    } else {
      "Peters' sample-size regression test"
    }
    predictor_text <- if (identical(input$biastest, "egger")) "standard error" else "inverse sample size"
    limit_vals <- analysis_to_display(c(test$est, test$ci.lb, test$ci.ub),
                                      input$datatype, input$measure, x_uses_natural_scale())

    line_note <- if (identical(input$biastest, "egger") && isTRUE(input$showTestLine)) {
      "<p>The Egger regression line is overlaid on the plot.</p>"
    } else if (identical(input$biastest, "peters")) {
      "<p class='small-note'>The Peters test uses inverse sample size as the regression predictor. Because the funnel y-axis is SE/variance/precision rather than sample size, the app reports the Peters regression result instead of drawing it as a standard funnel regression line.</p>"
    } else {
      ""
    }

    HTML(paste0(
      "<div class='result-block'>",
      "<p><b>Publication Bias Test</b></p>",
      "<p><b>", test_name, "</b> using ", predictor_text,
      " returned <b>", fmt_p_phrase(test$pval), "</b>.</p>",
      "<p>Limit estimate from this regression: <b>", fmt_num(limit_vals[1], plot_digits),
      "</b> with 95% CI <b>[", fmt_num(limit_vals[2], plot_digits), ", ",
      fmt_num(limit_vals[3], plot_digits), "]</b>.</p>",
      line_note,
      "</div>"
    ))
  })

  output$limitLineOutput <- renderUI({
    if (!identical(input$showLimit, "yes")) return(NULL)

    res <- model()
    validate(need(!is.null(res), "Waiting for a valid model..."))

    rows <- limit_line_estimates(res)
    if (length(rows) == 0) {
      return(HTML(paste0(
        "<div class='result-block'>",
        "<p><b>Limit Estimate Lines</b></p>",
        "<p>The selected limit-line estimate could not be calculated for this model.</p>",
        "</div>"
      )))
    }

    items <- vapply(rows, function(row) {
      paste0(
        "<li><b>", row$predictor_label, "</b> limit-line estimate ",
        row$location, ": <b>", row$effect_label, " = ",
        fmt_num(row$estimate, plot_digits), "</b> with 95% CI <b>[",
        fmt_num(row$ci.lb, plot_digits), ", ",
        fmt_num(row$ci.ub, plot_digits), "]</b>.</li>"
      )
    }, character(1))

    HTML(paste0(
      "<div class='result-block'>",
      "<p><b>Limit Estimate Lines</b></p>",
      "<ul>", paste(items, collapse = ""), "</ul>",
      "</div>"
    ))
  })

  output$trimfillInfo <- renderUI({
    if (!identical(input$trimfill, "yes")) return(NULL)

    tf <- trimfill_model()
    if (is.null(tf)) {
      return(HTML("<div class='result-block'><p style='color:red'><b>Trim-and-Fill</b></p><p>The trim-and-fill analysis could not be performed for this model.</p></div>"))
    }

    natural <- x_uses_natural_scale()
    vals <- analysis_to_display(c(tf$b, tf$ci.lb, tf$ci.ub),
                                input$datatype, input$measure, natural)
    label <- effect_label(input$datatype, input$measure, natural)

    HTML(paste0(
      "<div class='result-block'>",
      "<p><b>Trim-and-Fill Results</b></p>",
      "<p>Imputed studies (k0): <b>", tf$k0, "</b>.</p>",
      "<p>The adjusted pooled <b>", label, "</b> is <b>",
      fmt_num(vals[1], plot_digits), "</b> with 95% CI <b>[",
      fmt_num(vals[2], plot_digits), ", ", fmt_num(vals[3], plot_digits),
      "]</b>. The purple dashed line shows this adjusted estimate.</p>",
      "</div>"
    ))
  })

  output$regionInfo <- renderUI({
    if (identical(input$regionShading, "none")) return(NULL)

    if (identical(input$regionShading, "heterogeneity")) {
      if (!identical(input$method, "REML")) {
        return(HTML(paste0(
          "<div class='result-block'>",
          "<p><b>Region Shading</b></p>",
          "<p>The heterogeneity band is only drawn for the random-effects model, because it incorporates tau-squared.</p>",
          "</div>"
        )))
      }
      return(HTML(paste0(
        "<div class='result-block'>",
        "<p><b>Region Shading</b></p>",
        "<p>The heterogeneity band expands the pseudo confidence region by incorporating tau-squared from the random-effects model.</p>",
        "</div>"
      )))
    }

    counts <- pvalue_distribution()
    total <- counts$total
    pct <- function(x) round(100 * x / total)

    HTML(paste0(
      "<div class='result-block'>",
      "<p><b>Region Shading Results</b></p>",
      "<p>Contour shading shows study-level p-value regions around the null effect. Observed studies are counted by region below.</p>",
      "<ul>",
      "<li>0.10 &lt; p &lt;= 1.00: ", counts$nonsig, " studies (", pct(counts$nonsig), "%)</li>",
      "<li>0.05 &lt; p &lt;= 0.10: ", counts$border, " studies (", pct(counts$border), "%)</li>",
      "<li>0.01 &lt; p &lt;= 0.05: ", counts$high, " studies (", pct(counts$high), "%)</li>",
      "<li>0.00 &lt; p &lt;= 0.01: ", counts$veryhigh, " studies (", pct(counts$veryhigh), "%)</li>",
      "</ul>",
      "</div>"
    ))
  })

  output$studyKey <- renderUI({
    if (!identical(input$labelMode, "key")) return(NULL)

    items <- paste0("<li>", effect_data()$labels, "</li>")

    HTML(paste0(
      "<div class='result-block'>",
      "<p><b>Study Key</b></p>",
      "<ol>", paste(items, collapse = ""), "</ol>",
      "</div>"
    ))
  })

  study_key_lines <- function() {
    labels <- effect_data()$labels
    paste0(seq_along(labels), ". ", labels)
  }

  output$methodInfo <- renderUI({
    bits <- c(
      paste0(link("https://wviechtb.github.io/metafor/reference/funnel.html", "metafor funnel() reference"),
             " and ",
             link("https://www.metafor-project.org/doku.php/plots:funnel_plot_variations", "funnel plot variations")),
      link("https://pubmed.ncbi.nlm.nih.gov/11576817/", "Sterne and Egger (2001): choice of axis for funnel plots")
    )

    if (identical(input$regionShading, "contour")) {
      bits <- c(bits, link("https://pubmed.ncbi.nlm.nih.gov/18538991/", "Peters et al. (2008): contour-enhanced funnel plots"))
    }
    if (!identical(input$biastest, "none") &&
        !(identical(input$biastest, "peters") && !identical(input$datatype, "binary"))) {
      bits <- c(bits,
                link("https://wviechtb.github.io/metafor/reference/regtest.html", "metafor regtest() reference"),
                link("https://pubmed.ncbi.nlm.nih.gov/16418466/", "Peters et al. (2006): tests for publication bias"))
    }
    if (identical(input$showLimit, "yes")) {
      bits <- c(bits,
                link("https://www.metafor-project.org/doku.php/plots:funnel_plot_with_limit_estimate", "metafor limit-estimate funnel plot example"),
                link("https://pubmed.ncbi.nlm.nih.gov/22351645/", "Moreno et al. (2012): regression-based adjustment for small-study effects"))
    }
    if (identical(input$trimfill, "yes")) {
      bits <- c(bits,
                link("https://wviechtb.github.io/metafor/reference/trimfill.html", "metafor trimfill() reference"),
                link("https://doi.org/10.1111/j.0006-341X.2000.00455.x", "Duval and Tweedie (2000): trim and fill"))
    }

    HTML(paste0(
      "<div class='result-block method-links'>",
      "<p><b>Further Information</b></p>",
      "<ul><li>", paste(bits, collapse = "</li><li>"), "</li></ul>",
      "</div>"
    ))
  })

  analysis_results_lines <- function(res) {
    natural <- x_uses_natural_scale()
    label <- effect_label(input$datatype, input$measure, natural)
    vals <- analysis_to_display(c(res$b, res$ci.lb, res$ci.ub),
                                input$datatype, input$measure, natural)

    lines <- paste0(
      model_name(input$method), " pooled ", label, ": ",
      fmt_num(vals[1], plot_digits), " (95% CI ",
      fmt_num(vals[2], plot_digits), " to ",
      fmt_num(vals[3], plot_digits), ")."
    )

    if (identical(input$method, "REML")) {
      lines <- c(lines, paste0("Heterogeneity variance tau-squared: ",
                               fmt_num(res$tau2, plot_digits), "."))
    }

    if (!identical(input$biastest, "none") &&
        !(identical(input$biastest, "peters") && !identical(input$datatype, "binary"))) {
      test <- bias_test()
      if (!is.null(test)) {
        test_name <- if (identical(input$biastest, "egger")) "Egger's test" else "Peters' test"
        lines <- c(lines, paste0(test_name, ": ", fmt_p_phrase(test$pval, html = FALSE), "."))
      }
    }

    if (identical(input$showLimit, "yes")) {
      limit_rows <- limit_line_estimates(res)
      if (length(limit_rows) > 0) {
        limit_lines <- vapply(limit_rows, function(row) {
          paste0(row$predictor_label, " limit-line estimate ",
                 row$location, ": ", row$effect_label, " = ",
                 fmt_num(row$estimate, plot_digits), " (95% CI ",
                 fmt_num(row$ci.lb, plot_digits), " to ",
                 fmt_num(row$ci.ub, plot_digits), ").")
        }, character(1))
        lines <- c(lines, limit_lines)
      }
    }

    if (identical(input$trimfill, "yes")) {
      tf <- trimfill_model()
      if (!is.null(tf)) {
        tf_vals <- analysis_to_display(c(tf$b, tf$ci.lb, tf$ci.ub),
                                       input$datatype, input$measure, natural)
        lines <- c(lines, paste0("Trim-and-fill: k0 = ", tf$k0,
                                 "; adjusted ", label, " = ",
                                 fmt_num(tf_vals[1], plot_digits), " (95% CI ",
                                 fmt_num(tf_vals[2], plot_digits), " to ",
                                 fmt_num(tf_vals[3], plot_digits), ")."))
      }
    }

    if (identical(input$regionShading, "contour")) {
      lines <- c(lines, "Region shading: contour-enhanced p-value regions around the null effect.")
    } else if (identical(input$regionShading, "heterogeneity") && identical(input$method, "REML")) {
      lines <- c(lines, "Region shading: heterogeneity band incorporating tau-squared.")
    }

    lines
  }

  draw_results_footer <- function(res) {
    lines <- unlist(lapply(analysis_results_lines(res), strwrap, width = 115))
    lines <- lines[seq_len(min(length(lines), 10))]

    par(mar = c(0.2, 0.2, 0.2, 0.2))
    plot.new()
    title <- "Analysis results"
    text(0.02, 0.96, title, adj = c(0, 1), cex = 0.95, font = 2)

    y <- 0.82
    for (line in lines) {
      text(0.02, y, line, adj = c(0, 1), cex = 0.82)
      y <- y - 0.12
      if (y < 0.08) break
    }
  }

  draw_plot_legend_footer <- function(legend_info) {
    if (is.null(legend_info) || length(legend_info$labels) == 0) return()

    par(mar = c(0.1, 0.2, 0.1, 0.2))
    plot.new()
    legend("center",
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

  draw_study_key_footer <- function() {
    lines <- study_key_lines()
    n <- length(lines)
    split_at <- ceiling(n / 2)
    left <- lines[seq_len(split_at)]
    right <- if (split_at < n) lines[(split_at + 1):n] else character(0)

    par(mar = c(0.2, 0.2, 0.2, 0.2))
    plot.new()
    text(0.02, 0.96, "Study key", adj = c(0, 1), cex = 1, font = 2)

    draw_column <- function(x, values) {
      if (length(values) == 0) return()
      yvals <- seq(0.78, 0.12, length.out = length(values))
      for (i in seq_along(values)) {
        text(x, yvals[i], values[i], adj = c(0, 1), cex = 0.8)
      }
    }

    draw_column(0.02, left)
    draw_column(0.52, right)
  }

  output$downloadCurrentPlot <- downloadHandler(
    filename = function() {
      paste0("funnel_plot_", Sys.Date(), ".", tolower(input$formatCurrent))
    },
    content = function(file) {
      res <- model()
      if (is.null(res)) return()

      include_results <- isTRUE(input$downloadResults)
      include_key <- identical(input$labelMode, "key")
      extra_height <- 1.4
      extra_height <- extra_height + if (include_results) 2 else 0
      extra_height <- extra_height + if (include_key) 1.8 else 0
      height_in <- max(6, min(15, input$plotHeight / 80 + extra_height))
      if (identical(input$formatCurrent, "PDF")) {
        pdf(file, width = 9, height = height_in)
      } else {
        png(file, width = 9, height = height_in, units = "in", res = 300)
      }

      if (include_results || include_key) {
        heights <- c(4.8,
                     0.9,
                     if (include_key) 1.1 else NULL,
                     if (include_results) 1.25 else NULL)
        layout(matrix(seq_along(heights), ncol = 1), heights = heights)
        legend_info <- draw_funnel_plot(res, draw_legend = FALSE)
        draw_plot_legend_footer(legend_info)
        if (include_key) draw_study_key_footer()
        if (include_results) draw_results_footer(res)
        layout(1)
      } else {
        heights <- c(4.8, 0.9)
        layout(matrix(seq_along(heights), ncol = 1), heights = heights)
        legend_info <- draw_funnel_plot(res, draw_legend = FALSE)
        draw_plot_legend_footer(legend_info)
        layout(1)
      }
      dev.off()
    }
  )

  output$tabs_ui <- renderUI({
    plot_h <- paste0(input$plotHeight, "px")
    tags$div(
      h3("Funnel Plot"),
      plotOutput("funnelMain", height = plot_h),
      uiOutput("studyKey"),
      uiOutput("analysisSummary"),
      uiOutput("biasTestOutput"),
      uiOutput("limitLineOutput"),
      uiOutput("trimfillInfo"),
      uiOutput("regionInfo"),
      uiOutput("methodInfo"),
      tags$hr(),
      radioButtons("formatCurrent", "Download current plot as:",
                   choices = c("PDF", "PNG"), inline = TRUE),
      checkboxInput("downloadResults", "Include analysis results below downloaded plot", value = FALSE),
      downloadButton("downloadCurrentPlot", "Download Plot"),
      tags$hr()
    )
  })
}

# -----------------------------------------------------------------------------
# Run App
# -----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
