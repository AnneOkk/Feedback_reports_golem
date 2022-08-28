#' my_lm 
#'
#' @description A utils function
#' @import huxtable
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

my.lm <- function(data,
                  predictor,
                  dv,
                  controls,
                  row.name = predictor,
                  coef_names,
                  model_name, 
                  add.controls = NULL,
                  rm.controls = NULL,
                  caption = NULL) {
  model_output <-
    my.return.model.lm(data,
                       predictor,
                       dv,
                       controls,
                       coef_names,
                       model_name,
                       add.controls,
                       rm.controls)
  return(model_output)
}


my.return.model.lm <- function(data, predictor, dv, controls, coef_names, model_name, add.controls = NULL, rm.controls = NULL) {
  controls <- controls[!controls %in% rm.controls]
  controls <- paste(c(controls, add.controls), collapse = " + ")
  predictors <- paste(predictor, controls, sep = " + ")
  formula <- paste(dv, predictors, sep = " ~ ")
  model <- lm(formula, data = data)
  summary_model <- summary(model)
  coef_p <- summary_model$coefficients[,4][2] 
  if (coef_p > 0.05) {
    text_note = paste0("The effect of mean error severity is not significant. The 95% confidence interval includes zero.")
  } else {
    text_note = paste0("The effect of mean error severity is significant. The 95% confidence interval does not include zero.")
  }
  jtools::export_summs(model, coefs = coef_names, model.names = model_name, bold_signif = 0.05, statistics = c("Number of observations" = "nobs", "R squared" = "r.squared"), error_format = "CI [{conf.low}, {conf.high}]", stars = NULL, note = text_note)
}

