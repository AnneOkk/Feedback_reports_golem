#' regplot 
#'
#' @description A fct function
#' @import ggplot2
#' 
#' @return The return value, if any, from executing the function.
#'
#' @noRd

regplot <- function(data, x, y, partID) {
  plot <- ggplot(data, aes_string(x, y)) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") 
  plot
}
    
    

