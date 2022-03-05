#' barplot 
#'
#' @description This function returns a bar plot. 
#' @import ggplot2
#'
#' @return ggplot object 
#'
#' @noRd
  
barplot <- function(data, variable, x_name, y_name, age = FALSE, gender = FALSE, ...) {
  if(age == TRUE){
    part_age <- 44
    younger_than <- round(nrow(data[data[, variable] > part_age, ]) / (nrow(data) * 100), 0)
    text <- paste("At the time the survey was administered, you were younger than percent of the entrepreneurs \nin the sample.\n\n\n")
  } 
  ggplot2::ggplot(data, aes_string(variable))+
    ggtitle(text) +
    theme(text = element_text(size = 14)) +
    theme(plot.title = element_text(size=14)) +
    theme(axis.text.x = element_text(color = "black", size = 14, face = "plain")) +
    theme(axis.text.y = element_text(color = "black", size = 14, face = "plain")) +
    theme(legend.text = element_text(size = 14)) +
    geom_histogram(binwidth = 5, boundary = 0, closed = "right", fill="#21918c", color="#e9ecef") +
    scale_x_continuous(name = x_name, limits = c(15, 70)) +
    scale_y_continuous(name = y_name) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "white"),
          plot.background = element_blank(), 
          axis.text.x = element_text(colour = "white"),
          axis.text.y = element_text(colour = "white"),
          axis.title = element_text(color = "white"))
}


