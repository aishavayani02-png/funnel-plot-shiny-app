# Run from the repository root with Rscript tests/test-analysis.R.
app <- new.env()
shiny::loadSupport(".", renv = app, globalrenv = app)

local({
  binary <- fit_meta_model(prepare_effect_data("binary", "OR"), "FE")
  continuous <- fit_meta_model(prepare_effect_data("continuous", "MD"), "REML")
  stopifnot(inherits(binary, "rma.uni"), inherits(continuous, "rma.uni"))

  peters <- run_bias_test(binary, "peters", "binary")
  reference <- metafor::regtest(binary, model = "rma", predictor = "ninv")
  stopifnot(
    is.finite(peters$pval),
    isTRUE(all.equal(peters$pval, reference$pval)),
    is.null(run_bias_test(continuous, "peters", "continuous")),
    is.finite(run_bias_test(continuous, "egger", "continuous")$pval)
  )

  stopifnot(
    isTRUE(all.equal(select_refline(binary, "custom", 2, "binary", "OR", TRUE), log(2))),
    is.na(select_refline(binary, "custom", 0, "binary", "OR", TRUE)),
    identical(select_refline(continuous, "custom", -2, "continuous", "MD", FALSE), -2)
  )

  for (extrapolate in c(TRUE, FALSE)) {
    estimates <- limit_line_estimates(binary, "yes", c("sei", "vi"), extrapolate,
                                      "binary", "OR", TRUE)
    stopifnot(length(estimates) == 2L)
    for (row in estimates) {
      min_vi <- min(binary$vi)
      target <- if (extrapolate) {
        0
      } else if (row$predictor == "sei") {
        sqrt(min_vi)
      } else {
        min_vi
      }
      fit <- metafor::regtest(binary, model = "rma", predictor = row$predictor)$fit
      expected <- predict(fit, newmods = target)
      stopifnot(isTRUE(all.equal(
        c(row$estimate, row$ci.lb, row$ci.ub),
        exp(c(expected$pred, expected$ci.lb, expected$ci.ub))
      )))
    }
  }
}, envir = app)

message("Analysis checks passed.")
