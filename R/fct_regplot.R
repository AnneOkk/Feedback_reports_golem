#' regplot 
#'
#' @description A fct function
#' @import ggplot2
#' 
#' @return The return value, if any, from executing the function.
#'
#' @noRd

regplot <- function(data, x, y, partID, xname, yname) {
  breaks <- if(y == "t1jobsa"){
    c(-3, -2, -1, 0, 1)
  } else if(y == "t1jobstr"){
    c(1, 2, 3, 4)
  }
  labels <- if(y == "t1jobsa"){
    c("1", "2", "3", "4", "5")
  } else if(y == "t1jobstr"){
    c("1", "2", "3", "4")
  }
  part_x <- data[[x]][data[["ID_code"]] == partID]
  part_y <- data[[y]][data[["ID_code"]] == partID]
  plot <- ggplot2::ggplot(data, ggplot2::aes_string(x, y)) + 
    geom_point(colour = "white") +
    stat_smooth(method = "lm", col = "red") +
    geom_point(aes(x = part_x, y = part_y), colour = "red", size = 8) + 
    theme(plot.title = element_text(size = 14)) +
    theme(axis.text.x = element_text(color = "black", size = 14, face = "plain")) +
    theme(axis.text.y = element_text(color = "black", size = 14, face = "plain")) +
    theme(legend.text = element_text(size = 14)) +
    scale_x_continuous(name = xname, breaks = c(2, 3, 4, 5, 6, 7), labels = c("1", "2", "3", "4", "5", "6")) +
    scale_y_continuous(name = yname, breaks = breaks, labels = labels) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "white"),
          plot.background = element_blank(), 
          axis.text.x = element_text(colour = "white"),
          axis.text.y = element_text(colour = "white"),
          axis.title = element_text(color = "white"))
  plot
}
    
    

