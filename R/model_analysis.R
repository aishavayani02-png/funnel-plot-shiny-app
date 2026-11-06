#' Calculate study effects for the selected example dataset
#' @param datatype Outcome type, "binary" or "continuous".
#' @param measure Effect measure passed to metafor::escalc().
#' @return A list with the source data, calculated effects and study labels.
prepare_effect_data <- function(datatype, measure) {
  if (identical(datatype, "binary")) {
    dat <- metadat::dat.bcg
    esc <- metafor::escalc(measure = measure,
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
    esc <- metafor::escalc(measure = measure,
                           m1i = dat$m1i, sd1i = dat$sd1i, n1i = dat$n1i,
                           m2i = dat$m2i, sd2i = dat$sd2i, n2i = dat$n2i,
                           data = dat)
    labels <- if ("study" %in% names(dat)) {
      paste("Study", dat$study)
    } else {
      rownames(dat)
    }
  }

  return(list(dat = dat, esc = esc, labels = labels))
}

#' Fit the pooled meta-analysis model
#' @param effect_data List returned by prepare_effect_data().
#' @param method Model fitting method, "FE" or "REML".
#' @return A metafor fit, or NULL if fitting fails.
fit_meta_model <- function(effect_data, method) {
  return(tryCatch(
    metafor::rma(yi, vi, data = effect_data$esc, method = method),
    error = function(e) NULL
  ))
}

#' Convert a displayed effect to the model scale
#' @param x Numeric value on the display scale.
#' @inheritParams effect_label
#' @return The value on the model scale.
display_to_analysis <- function(x, datatype, measure, natural_scale = TRUE) {
  if (is_log_effect(datatype, measure) && natural_scale) {
    return(log(x))
  }
  return(x)
}

#' Find the reference line on the model scale
#' @param res Fitted meta-analysis model.
#' @param refline_mode Reference line choice, "none", "pooled" or "custom".
#' @param custom_refline User-entered value on the display scale.
#' @inheritParams effect_label
#' @return The line position, or NA when the line should be omitted.
select_refline <- function(res, refline_mode, custom_refline,
                           datatype, measure, natural_scale) {
  if (is.null(res)) {
    return(NA_real_)
  }
  if (identical(refline_mode, "none")) {
    return(NA_real_)
  }
  if (identical(refline_mode, "pooled")) {
    return(as.numeric(stats::coef(res))[1])
  }

  custom_value <- suppressWarnings(as.numeric(custom_refline))
  if (!is.finite(custom_value)) {
    return(NA_real_)
  }
  if (is_log_effect(datatype, measure) && natural_scale && custom_value <= 0) {
    return(NA_real_)
  }

  return(display_to_analysis(custom_value, datatype, measure, natural_scale))
}

#' Fit a trim-and-fill model when requested
#' @param res Fitted meta-analysis model.
#' @param trimfill_enabled Whether trim-and-fill was selected ("yes" or "no").
#' @return The adjusted fit, or NULL if disabled or unavailable.
run_trimfill <- function(res, trimfill_enabled) {
  if (!identical(trimfill_enabled, "yes") || is.null(res)) {
    return(NULL)
  }
  return(tryCatch(metafor::trimfill(res), error = function(e) NULL))
}

#' Run the selected test for funnel plot asymmetry
#' @param res Fitted meta-analysis model.
#' @param biastest Test code, "egger", "peters" or "none".
#' @param datatype Outcome type. Peters' test is available for binary outcomes.
#' @return The metafor regression test result, or NULL when unavailable.
run_bias_test <- function(res, biastest, datatype) {
  if (is.null(res) || identical(biastest, "none")) {
    return(NULL)
  }
  if (identical(biastest, "peters") && !identical(datatype, "binary")) {
    return(NULL)
  }

  return(tryCatch({
    if (identical(biastest, "egger")) {
      metafor::regtest(res, model = "rma", predictor = "sei")
    } else {
      metafor::regtest(res, model = "rma", predictor = "ninv")
    }
  }, error = function(e) NULL))
}

#' Count studies in each p-value contour region
#' @param res Fitted meta-analysis model with study effects and variances.
#' @return A list with the total and the counts at the contour cut-offs.
study_pvalue_distribution <- function(res) {
  p_vals <- 2 * stats::pnorm(-abs(res$yi / sqrt(res$vi)))
  return(list(
    total = length(p_vals),
    veryhigh = sum(p_vals <= 0.01),
    high = sum(p_vals > 0.01 & p_vals <= 0.05),
    border = sum(p_vals > 0.05 & p_vals <= 0.10),
    nonsig = sum(p_vals > 0.10)
  ))
}

#' Predict an effect at a given precision
#' @param fit Fitted meta-regression.
#' @param target Standard error or variance at which to predict.
#' @return The prediction and confidence limits, or NULL if unavailable.
predict_limit_value <- function(fit, target) {
  pred <- tryCatch(stats::predict(fit, newmods = target), error = function(e) NULL)
  if (!is.null(pred) && all(c("pred", "ci.lb", "ci.ub") %in% names(pred))) {
    return(as.numeric(c(pred$pred, pred$ci.lb, pred$ci.ub)))
  }

  b <- as.numeric(stats::coef(fit))
  V <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  if (length(b) < 2 || is.null(V) || nrow(V) < 2 || ncol(V) < 2) {
    return(NULL)
  }

  x <- c(1, target)
  est <- sum(x * b[1:2])
  se <- sqrt(drop(t(x) %*% V[1:2, 1:2] %*% x))
  if (!is.finite(est) || !is.finite(se)) {
    return(NULL)
  }

  crit <- stats::qnorm(0.975)
  return(c(est, est - crit * se, est + crit * se))
}

#' Estimate the effect at the end of each limit line
#' @param res Fitted meta-analysis model.
#' @param show_limit Whether limit lines were selected ("yes" or "no").
#' @param limit_predictors Predictor codes ("sei" and/or "vi").
#' @param limit_extrapolate Whether to predict at zero rather than at the most precise study.
#' @param digits Decimal places for the predictor in the location label.
#' @inheritParams effect_label
#' @return A list of estimates, confidence limits and labels on the display scale.
limit_line_estimates <- function(res, show_limit, limit_predictors,
                                 limit_extrapolate, datatype, measure,
                                 natural_scale, digits = plot_digits) {
  if (!identical(show_limit, "yes") || is.null(res)) {
    return(list())
  }

  predictors <- intersect(c("sei", "vi"), limit_predictors)
  if (length(predictors) == 0) {
    return(list())
  }

  se_obs <- sqrt(res$vi)
  se_obs <- se_obs[is.finite(se_obs) & se_obs > 0]
  if (length(se_obs) < 2) {
    return(list())
  }

  min_se <- min(se_obs)
  label <- effect_label(datatype, measure, natural_scale)

  rows <- lapply(predictors, function(predictor) {
    test <- tryCatch(
      metafor::regtest(res, model = "rma", predictor = predictor),
      error = function(e) NULL
    )
    if (is.null(test) || is.null(test$fit)) {
      return(NULL)
    }

    if (identical(predictor, "vi")) {
      target <- if (isTRUE(limit_extrapolate)) {
        0
      } else {
        min_se^2
      }
      location <- if (isTRUE(limit_extrapolate)) {
        "extrapolated to sampling variance = 0"
      } else {
        paste0("at the most precise observed study (sampling variance = ",
               fmt_num(min_se^2, digits), ")")
      }
      predictor_label <- "Sampling variance"
    } else {
      target <- if (isTRUE(limit_extrapolate)) {
        0
      } else {
        min_se
      }
      location <- if (isTRUE(limit_extrapolate)) {
        "extrapolated to SE = 0"
      } else {
        paste0("at the most precise observed study (SE = ",
               fmt_num(min_se, digits), ")")
      }
      predictor_label <- "Standard error"
    }

    vals <- predict_limit_value(test$fit, target)
    if (is.null(vals) || any(!is.finite(vals))) {
      return(NULL)
    }
    vals <- analysis_to_display(vals, datatype, measure, natural_scale)

    return(list(
      predictor = predictor,
      predictor_label = predictor_label,
      location = location,
      effect_label = label,
      estimate = vals[1],
      ci.lb = vals[2],
      ci.ub = vals[3]
    ))
  })

  return(Filter(Negate(is.null), rows))
}
