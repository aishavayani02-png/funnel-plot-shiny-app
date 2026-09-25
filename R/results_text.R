# Text and HTML generation for app outputs and downloaded plot footers.

analysis_summary_html <- function(res, datatype, measure, method,
                                  natural_scale, refline_mode) {
  vals <- analysis_to_display(c(res$b, res$ci.lb, res$ci.ub),
                              datatype, measure, natural_scale)
  label <- effect_label(datatype, measure, natural_scale)
  tau_text <- if (identical(method, "REML")) {
    paste0(" The estimated heterogeneity variance tau-squared is <b>",
           fmt_num(res$tau2, 3), "</b>.")
  } else {
    ""
  }

  ref_text <- switch(refline_mode,
                     "pooled" = "The vertical blue line shows the current pooled estimate.",
                     "custom" = "The vertical blue line shows the user-defined reference value.",
                     "none" = "No vertical reference line is currently shown.")

  paste0(
    "<div class='result-block'>",
    "<p><b>Current Analysis</b></p>",
    "<p>", model_name(method), " pooled <b>", label, "</b>: <b>",
    fmt_num(vals[1], plot_digits), "</b> with 95% CI <b>[",
    fmt_num(vals[2], plot_digits), ", ",
    fmt_num(vals[3], plot_digits), "]</b>.", tau_text, "</p>",
    "<p>", ref_text, " The funnel confidence region is fixed at 95% for the standard plot.</p>",
    "</div>"
  )
}

bias_test_html <- function(test, biastest, datatype, measure,
                           natural_scale, show_test_line) {
  if (is.null(test)) {
    return(paste0(
      "<div class='result-block'>",
      "<p style='color:red'><b>Publication Bias Test</b></p>",
      "<p>The selected test could not be performed for this model.</p>",
      "</div>"
    ))
  }

  test_name <- if (identical(biastest, "egger")) {
    "Egger's regression test"
  } else {
    "Peters' sample-size regression test"
  }
  predictor_text <- if (identical(biastest, "egger")) "standard error" else "inverse sample size"
  limit_vals <- analysis_to_display(c(test$est, test$ci.lb, test$ci.ub),
                                    datatype, measure, natural_scale)

  line_note <- if (identical(biastest, "egger") && isTRUE(show_test_line)) {
    "<p>The Egger regression line is overlaid on the plot.</p>"
  } else if (identical(biastest, "peters")) {
    "<p class='small-note'>The Peters test uses inverse sample size as the regression predictor. Because the funnel y-axis is SE/variance/precision rather than sample size, the app reports the Peters regression result instead of drawing it as a standard funnel regression line.</p>"
  } else {
    ""
  }

  paste0(
    "<div class='result-block'>",
    "<p><b>Publication Bias Test</b></p>",
    "<p><b>", test_name, "</b> using ", predictor_text,
    " returned <b>", fmt_p_phrase(test$pval), "</b>.</p>",
    "<p>Limit estimate from this regression: <b>", fmt_num(limit_vals[1], plot_digits),
    "</b> with 95% CI <b>[", fmt_num(limit_vals[2], plot_digits), ", ",
    fmt_num(limit_vals[3], plot_digits), "]</b>.</p>",
    line_note,
    "</div>"
  )
}

limit_line_html <- function(limit_rows) {
  if (length(limit_rows) == 0) {
    return(paste0(
      "<div class='result-block'>",
      "<p><b>Limit Estimate Lines</b></p>",
      "<p>The selected limit-line estimate could not be calculated for this model.</p>",
      "</div>"
    ))
  }

  items <- vapply(limit_rows, function(row) {
    paste0(
      "<li><b>", row$predictor_label, "</b> limit-line estimate ",
      row$location, ": <b>", row$effect_label, " = ",
      fmt_num(row$estimate, plot_digits), "</b> with 95% CI <b>[",
      fmt_num(row$ci.lb, plot_digits), ", ",
      fmt_num(row$ci.ub, plot_digits), "]</b>.</li>"
    )
  }, character(1))

  paste0(
    "<div class='result-block'>",
    "<p><b>Limit Estimate Lines</b></p>",
    "<ul>", paste(items, collapse = ""), "</ul>",
    "</div>"
  )
}

trimfill_html <- function(trimfill_model, datatype, measure, natural_scale) {
  if (is.null(trimfill_model)) {
    return(paste0(
      "<div class='result-block'>",
      "<p style='color:red'><b>Trim-and-Fill</b></p>",
      "<p>The trim-and-fill analysis could not be performed for this model.</p>",
      "</div>"
    ))
  }

  vals <- analysis_to_display(c(trimfill_model$b, trimfill_model$ci.lb, trimfill_model$ci.ub),
                              datatype, measure, natural_scale)
  label <- effect_label(datatype, measure, natural_scale)

  paste0(
    "<div class='result-block'>",
    "<p><b>Trim-and-Fill Results</b></p>",
    "<p>Imputed studies (k0): <b>", trimfill_model$k0, "</b>.</p>",
    "<p>The adjusted pooled <b>", label, "</b> is <b>",
    fmt_num(vals[1], plot_digits), "</b> with 95% CI <b>[",
    fmt_num(vals[2], plot_digits), ", ", fmt_num(vals[3], plot_digits),
    "]</b>. The purple dashed line shows this adjusted estimate.</p>",
    "</div>"
  )
}

region_info_html <- function(region_shading, method, counts = NULL) {
  if (identical(region_shading, "none")) return(NULL)

  if (identical(region_shading, "heterogeneity")) {
    if (!identical(method, "REML")) {
      return(paste0(
        "<div class='result-block'>",
        "<p><b>Region Shading</b></p>",
        "<p>The heterogeneity band is only drawn for the random-effects model, because it incorporates tau-squared.</p>",
        "</div>"
      ))
    }
    return(paste0(
      "<div class='result-block'>",
      "<p><b>Region Shading</b></p>",
      "<p>The heterogeneity band expands the pseudo confidence region by incorporating tau-squared from the random-effects model.</p>",
      "</div>"
    ))
  }

  total <- counts$total
  pct <- function(x) round(100 * x / total)

  paste0(
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
  )
}

study_key_html <- function(labels) {
  items <- paste0("<li>", labels, "</li>")
  paste0(
    "<div class='result-block'>",
    "<p><b>Study Key</b></p>",
    "<ol>", paste(items, collapse = ""), "</ol>",
    "</div>"
  )
}

method_info_html <- function(region_shading, biastest, datatype, show_limit, trimfill) {
  bits <- c(
    paste0(link("https://wviechtb.github.io/metafor/reference/funnel.html", "metafor funnel() reference"),
           " and ",
           link("https://www.metafor-project.org/doku.php/plots:funnel_plot_variations", "funnel plot variations")),
    link("https://pubmed.ncbi.nlm.nih.gov/11576817/", "Sterne and Egger (2001): choice of axis for funnel plots")
  )

  if (identical(region_shading, "contour")) {
    bits <- c(bits, link("https://pubmed.ncbi.nlm.nih.gov/18538991/", "Peters et al. (2008): contour-enhanced funnel plots"))
  }
  if (!identical(biastest, "none") &&
      !(identical(biastest, "peters") && !identical(datatype, "binary"))) {
    bits <- c(bits,
              link("https://wviechtb.github.io/metafor/reference/regtest.html", "metafor regtest() reference"),
              link("https://pubmed.ncbi.nlm.nih.gov/16418466/", "Peters et al. (2006): tests for publication bias"))
  }
  if (identical(show_limit, "yes")) {
    bits <- c(bits,
              link("https://www.metafor-project.org/doku.php/plots:funnel_plot_with_limit_estimate", "metafor limit-estimate funnel plot example"),
              link("https://pubmed.ncbi.nlm.nih.gov/22351645/", "Moreno et al. (2012): regression-based adjustment for small-study effects"))
  }
  if (identical(trimfill, "yes")) {
    bits <- c(bits,
              link("https://wviechtb.github.io/metafor/reference/trimfill.html", "metafor trimfill() reference"),
              link("https://doi.org/10.1111/j.0006-341X.2000.00455.x", "Duval and Tweedie (2000): trim and fill"))
  }

  paste0(
    "<div class='result-block method-links'>",
    "<p><b>Further Information</b></p>",
    "<ul><li>", paste(bits, collapse = "</li><li>"), "</li></ul>",
    "</div>"
  )
}

analysis_results_lines <- function(res, datatype, measure, method, natural_scale,
                                   biastest, bias_test, show_limit, limit_rows,
                                   trimfill, trimfill_model, region_shading) {
  label <- effect_label(datatype, measure, natural_scale)
  vals <- analysis_to_display(c(res$b, res$ci.lb, res$ci.ub),
                              datatype, measure, natural_scale)

  lines <- paste0(
    model_name(method), " pooled ", label, ": ",
    fmt_num(vals[1], plot_digits), " (95% CI ",
    fmt_num(vals[2], plot_digits), " to ",
    fmt_num(vals[3], plot_digits), ")."
  )

  if (identical(method, "REML")) {
    lines <- c(lines, paste0("Heterogeneity variance tau-squared: ",
                             fmt_num(res$tau2, plot_digits), "."))
  }

  if (!identical(biastest, "none") && !is.null(bias_test)) {
    test_name <- if (identical(biastest, "egger")) "Egger's test" else "Peters' test"
    lines <- c(lines, paste0(test_name, ": ", fmt_p_phrase(bias_test$pval, html = FALSE), "."))
  }

  if (identical(show_limit, "yes") && length(limit_rows) > 0) {
    limit_lines <- vapply(limit_rows, function(row) {
      paste0(row$predictor_label, " limit-line estimate ",
             row$location, ": ", row$effect_label, " = ",
             fmt_num(row$estimate, plot_digits), " (95% CI ",
             fmt_num(row$ci.lb, plot_digits), " to ",
             fmt_num(row$ci.ub, plot_digits), ").")
    }, character(1))
    lines <- c(lines, limit_lines)
  }

  if (identical(trimfill, "yes") && !is.null(trimfill_model)) {
    tf_vals <- analysis_to_display(c(trimfill_model$b, trimfill_model$ci.lb, trimfill_model$ci.ub),
                                   datatype, measure, natural_scale)
    lines <- c(lines, paste0("Trim-and-fill: k0 = ", trimfill_model$k0,
                             "; adjusted ", label, " = ",
                             fmt_num(tf_vals[1], plot_digits), " (95% CI ",
                             fmt_num(tf_vals[2], plot_digits), " to ",
                             fmt_num(tf_vals[3], plot_digits), ")."))
  }

  if (identical(region_shading, "contour")) {
    lines <- c(lines, "Region shading: contour-enhanced p-value regions around the null effect.")
  } else if (identical(region_shading, "heterogeneity") && identical(method, "REML")) {
    lines <- c(lines, "Region shading: heterogeneity band incorporating tau-squared.")
  }

  lines
}
