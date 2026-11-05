# Shared helper functions that do not depend on Shiny.

#' Check whether the model uses log effects
#' @param datatype Outcome type.
#' @param measure Effect measure code.
#' @return TRUE for binary odds ratios and risk ratios.
is_log_effect <- function(datatype, measure) {
  return(identical(datatype, "binary") && measure %in% c("OR", "RR"))
}

#' Format a number for display
#' @param x Numeric value to format.
#' @param digits Number of decimal places.
#' @return A character string, or "NA" for a non-finite value.
fmt_num <- function(x, digits = 2) {
  x <- as.numeric(x)[1]
  if (!is.finite(x)) {
    return("NA")
  }
  return(formatC(x, format = "f", digits = digits))
}

#' Label an effect measure
#' @param datatype Outcome type.
#' @param measure Effect measure code.
#' @param natural_scale Whether ratios are shown on their natural scale.
#' @return A label for the plot and results.
effect_label <- function(datatype, measure, natural_scale = TRUE) {
  return(switch(measure,
                "OR" = if (natural_scale) {
                  "Odds Ratio"
                } else {
                  "Log Odds Ratio"
                },
                "RR" = if (natural_scale) {
                  "Risk Ratio"
                } else {
                  "Log Risk Ratio"
                },
                "RD" = "Risk Difference",
                "MD" = "Mean Difference",
                "SMD" = "Standardized Mean Difference",
                "Effect"))
}

#' Convert estimates to the display scale
#' @param x Numeric vector on the model scale.
#' @inheritParams effect_label
#' @return The estimates on the requested display scale.
analysis_to_display <- function(x, datatype, measure, natural_scale = TRUE) {
  if (is_log_effect(datatype, measure) && natural_scale) {
    return(exp(x))
  }
  return(x)
}

#' Label the meta-analysis model
#' @param method Model fitting method.
#' @return The model name used in titles and results.
model_name <- function(method) {
  if (identical(method, "REML")) {
    return("Random-effects (REML)")
  }
  return("Fixed-effects")
}
