#' gender_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import dplyr tibble viridis
#' @importFrom plotly plotlyOutput renderPlotly plot_ly

mod_gender_plot_ui <- function(id){
  plotly::plotlyOutput(NS(id, "gender_plot")
  )
}
    
#' gender_plot Server Functions
#'
#' @noRd 
mod_gender_plot_server <- function(id, df){
  moduleServer(
    id, 
    function(input, output, session){
      ns <- session$ns
      output$gender_plot <- plotly::renderPlotly({
        lbls_gender <- c("Male", "Female", "Non-binary")
        count_gender <- df %>%
          group_by(Gender) %>%
          dplyr::tally() %>%
          .[2] %>%
          .[1:3, ] %>%
          unlist(.)
        
        df_gender <- tibble::as_tibble(cbind(count_gender, lbls_gender))
        plotly::plot_ly(df_gender, labels = ~lbls_gender, values = ~count_gender,            
                marker = list(colors = viridis::viridis_pal(option = "D")(3),
                              line = list(color = '#FFFFFF', width = 1)), 
                type = "pie", width = 500, height = 500) %>%
          plotly::layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                 paper_bgcolor = "rgba(0, 0, 0, 0)", 
                 font = list(size = 14,
                             color = "white"))
      })
    })
}
library(dplyr)
    
## To be copied in the UI
# mod_gender_plot_ui("gender_plot_ui_1")
    
## To be copied in the server
# mod_gender_plot_server("gender_plot_ui_1")
# 
# 

