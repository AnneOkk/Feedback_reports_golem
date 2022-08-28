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
  jtools::export_summs(model, coefs = coef_names, model.names = model_name, error_format = "(SE = {statistic}, p = {p.value})")
}

