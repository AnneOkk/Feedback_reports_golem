#' barplot 
#'
#' @description This function returns a bar plot. 
#' @import ggplot2
#'
#' @return ggplot object 
#' @examples 
#' barplot(data = df, variable = "t1age_1", x_name = "Age", y_name = "Number", age = TRUE, partID = 1001)
#'
#' @noRd

# create base plot 
barplot <- function(data, variable, x_name, y_name, age = FALSE, gender = FALSE, partID) {
  plot <- ggplot2::ggplot(data, aes_string(variable))+
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
  # add age info
  if(age == TRUE){
    part_age <- data[[variable]][data[["ID_code"]] == partID]
    younger_than <- round((nrow(data[data[, variable] > part_age, ]) / nrow(data[data[, variable] > 18, ])) * 100, 0)
    text <- paste(" You are younger \n than", younger_than, " percent\n of the participants")
    plot + 
      theme(text = element_text(size = 14, colour = "white")) +
      geom_vline(xintercept = part_age, linetype="dotted", color = "white", size=1.5) + annotate(geom="text", x=part_age, y=40, label=text,
                        color="white", hjust = 0)
    } 
  }
    


