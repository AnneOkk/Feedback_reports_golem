#' age_hist UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @import ggplot2

mod_age_hist_ui <- function(id){
  plotOutput(NS(id, "age_plot")
  )
}
    
#' age_hist Server Functions
#'
#' @noRd 
mod_age_hist_server <-  function(id, df, partID){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$age_plot <- renderPlot({
      partID <- partID()
      barplot(data = df, variable = "t1age", x_name = "Age", y_name = "Count", age = TRUE, gender = FALSE, partID)
    }, bg="transparent")
  })
}



    

## To be copied in the UI
# mod_age_hist_ui("age_hist_ui_1")
    
## To be copied in the server
# mod_age_hist_server("age_hist_ui_1")
# 

