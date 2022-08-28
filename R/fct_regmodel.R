#' regmodel 
#'
#' @description A fct function
#' @import jtools papaja huxtable
#' @return The return value, if any, from executing the function.
#'
#' @noRd

regmodel <- function(data, x, y, controls, coef_names, model_name) {
  my.lm(data = data,
        dv = y,
        predictor = x,
        controls = controls, 
        coef_names = coef_names,
        model_name = model_name)
}
  