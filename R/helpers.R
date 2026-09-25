# Shared helper functions that do not depend on Shiny.

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
